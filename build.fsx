#r "paket:
nuget Fake.Core.Target 
nuget Fake.StaticGen
nuget Fake.StaticGen.Html
nuget Fake.StaticGen.Markdown
nuget MarkdigExtensions.SyntaxHighlighting
nuget MarkdigExtensions.UrlRewriter
nuget Nett
nuget NetCoreVersions //"
#load "./.fake/build.fsx/intellisense.fsx"
#if !FAKE
    #r "Facades/netstandard" // Intellisense fix, see FAKE #1938
    #r "netstandard"
#endif

open Fake.Core
open Fake.IO
open Fake.IO.Globbing.Operators
open Fake.StaticGen
open Fake.StaticGen.Html
open Fake.StaticGen.Html.ViewEngine
open Fake.StaticGen.Markdown

open System
open System.IO
open System.Net.Http

open Markdig
open NetCore.Versions
open NetCore.Versions.Data
open Nett
open Thoth.Json.Net

// Helpers
//////////
module private Async =
    let map f x = async.Bind(x, f >> async.Return)

module private Result =
    let rec allOk xs =
        match xs with
        | [] -> Ok []
        | Ok x :: rest -> Result.map (fun r -> x::r) (allOk rest)
        | Error x :: _ -> Error x

module private Version =
    let simplify version =
        match version.Preview with
        | Some prev -> { version with Preview = Some (prev.Split('-').[0]) }
        | None -> version

// Data dowloads
////////////////
let download (accept : string) (url : string) =
    async {
        use http = new HttpClient()
        use req = new HttpRequestMessage(HttpMethod.Get, url)
        req.Headers.Add("Accept", accept)
        Trace.tracefn "Downloading %s" url
        let! resp = http.SendAsync req |> Async.AwaitTask
        if resp.IsSuccessStatusCode then
            let! content = resp.Content.ReadAsStringAsync() |> Async.AwaitTask
            return Ok content
        else
            return Error (sprintf "Error fetching %s, response status %O" url resp.StatusCode)
    }

let getJson url =
    download "application/json" url

let rewriteGithubUrls (url : string) =
    if url.StartsWith("https://github.com") 
    then url.Replace("/blob/", "/raw/") 
    else url

let downloadGh accept = rewriteGithubUrls >> download accept
let getJsonGh = rewriteGithubUrls >> getJson

let decodeIndex = Decode.fromString (Decode.field "releases-index" (Decode.list IndexEntry.Decoder))
let decodeChannel = Decode.fromString Channel.Decoder

let tryGetIndex url =
    async { let! json = getJsonGh url in return json |> Result.bind decodeIndex }

let tryGetChannel url =
    async { let! json = getJsonGh url in return json |> Result.bind decodeChannel }

let tryGetChannelsForIndex = 
    List.map (fun i -> tryGetChannel i.ReleasesJson)
    >> Async.Parallel
    >> Async.map (List.ofArray >> Result.allOk)

let tryGetChannels indexUrl =
    async {
        match! tryGetIndex indexUrl with
        | Ok index -> return! tryGetChannelsForIndex index
        | Error e -> return Error e   
    }

let getMdReleaseNoteLinks channels =
    [ for ch in channels do yield! ch.Releases ]
    |> List.choose (fun rel -> 
        match rel.ReleaseNotes with
        | Some url when url.EndsWith(".md") -> Some url
        | _ -> None)

let tryGetReleaseNotes channels =
    let releases = getMdReleaseNoteLinks channels
    [ for url in releases ->
        async {
            let! md = downloadGh "text/plain" url
            return Result.map (fun md -> Some url, md) md
        } ]
    |> Async.Parallel
    |> Async.map (List.ofArray >> Result.allOk >> Result.map Map.ofList)

// Data Helpers
///////////////
let allSdks rel = rel.Sdk :: rel.Sdks |> List.distinct

let getLatestRuntimeRel channel = 
    channel.Releases 
    |> List.find (fun rel -> 
        rel.Runtime 
        |> Option.map (fun rt -> rt.Version = channel.LatestRuntime) 
        |> Option.defaultValue false)

let getLatestSdkRel channel = 
    channel.Releases 
    |> List.find (fun rel -> 
        allSdks rel 
        |> List.exists (fun sdk -> sdk.Version = channel.LatestSdk))

// Site structure and page creation
///////////////////////////////////
type CoreInfo =
    { LatestRuntime : Version
      LatestRuntimeUrl : Url
      LatestSdk : Version
      LatestSdkUrl : Url
      PrimaryChannels : Channel list }

type CorePage =
    | ChannelsOverview of Channel list
    | ChannelPage of Channel
    | ReleasePage of {| Channel : Channel; Release : Release; ReleaseNotesMarkdown : string option |}

type Page =
    | HomePage of CoreInfo
    | CorePage of CorePage
    | ErrorPage of code: string * text: string

// TODO: index.html should not be needed here, fix in Fake.StaticGen
let channelUrl ch = sprintf "/core/%O/" ch.ChannelVersion
let releaseUrl ch rel = sprintf "/core/%O/%O/" ch.ChannelVersion rel.ReleaseVersion

let channelsToPages channels releaseNotesMap =
    [ yield { Url = "/core/"; Content = ChannelsOverview channels }
      for ch in channels do
        yield { Url = channelUrl ch; Content = ChannelPage ch }
        for rel in ch.Releases do
            yield { Url = releaseUrl ch rel
                    Content = ReleasePage {| Channel = ch
                                             Release = rel
                                             ReleaseNotesMarkdown = releaseNotesMap |> Map.tryFind rel.ReleaseNotes |} } ]
    |> List.map (fun cp -> { Url = cp.Url; Content = CorePage cp.Content })

let getHomePage channels =
    let current = channels |> List.find (fun ch -> ch.SupportPhase = "current")
    let coreInfo =
        { LatestRuntime = current.LatestRuntime
          LatestRuntimeUrl = releaseUrl current (getLatestRuntimeRel current)
          LatestSdk = current.LatestSdk
          LatestSdkUrl = releaseUrl current (getLatestSdkRel current)
          PrimaryChannels = channels |> List.filter (fun ch -> ch.SupportPhase <> "eol") }
    { Url = "/"; Content = HomePage coreInfo }

let tryGetPages indexUrl = 
    async {
        match! tryGetChannels indexUrl with
        | Ok channels ->
            let! releaseNotesMap = tryGetReleaseNotes channels
            let corePages = Result.map (channelsToPages channels) releaseNotesMap
            let homePage = getHomePage channels
            return Result.map (fun cps -> homePage::cps) corePages
        | Error e -> 
            return Error e
    }

let getBreadcrumbs = 
    let home = (".NET", "/")
    function
    | HomePage _ -> []
    | CorePage x ->
        let ov = (".NET Core", "/core/")
        home ::
        match x with
        | ChannelsOverview _ -> []
        | ChannelPage _ -> [ ov ]
        | ReleasePage rel -> [ ov; (sprintf "Channel %O" rel.Channel.ChannelVersion, channelUrl rel.Channel) ]
    | ErrorPage _ -> []

// Site configuration
/////////////////////
let now = DateTime.UtcNow

type Config =
    { ReleasesIndexUrl : string
      Title : string
      Description : string }

let parseConfig config =
    let toml = Toml.ReadString(config)
    { ReleasesIndexUrl = toml.["releases-index-url"].Get()
      Title = toml.["title"].Get()
      Description = toml.["description"].Get() }

// Template
///////////
let template (site : StaticSite<Config, Page>) page = 
    let _property = XmlEngine.attr "property"
    let date () (d : DateTime) = d.ToLocalTime().ToString "yyyy-MM-dd" 

    let mdPipeline (url : string) =
        MarkdownPipelineBuilder()
            .UsePipeTables()
            .UseAutoLinks()
            .UseAutoIdentifiers(Extensions.AutoIdentifiers.AutoIdentifierOptions.GitHub)
            .UseSyntaxHighlighting()
            .UseUrlRewriter(fun link -> 
                if Uri.IsWellFormedUriString(link.Url, UriKind.Absolute) || link.Url.StartsWith("#") then
                    link.Url
                else
                    url.Substring(0, url.LastIndexOf('/')) + "/" + link.Url)
            .Build()

    let indicatorSymb symb text clas = 
        div [ _class "status-box" ] [ 
            span [ _class ("status-indicator " + clas)
                   _title text ] 
                 symb 
            span [] [ str text ] 
        ]

    let supportIndicator supportPhase =
        let indicator = indicatorSymb []
        match supportPhase with
        | "preview" ->     indicator "Preview" "border-black"
        | "current" ->     indicator "Current" "green"
        | "lts" ->         indicator "Long Term Support" "yellow"
        | "eol" ->         indicator "End of Life" "red"
        | "maintenance" -> indicator "Maintenance" "yellow"
        | t -> indicatorSymb [ str "?" ] t "border-black"

    let titleText =
        match page.Content with
        | HomePage _ -> ""
        | CorePage (ChannelsOverview _) -> ".NET Core - "
        | CorePage (ChannelPage ch) -> sprintf "Channel %O - .NET Core - " ch.ChannelVersion
        | CorePage (ReleasePage rel) -> sprintf "Release %O - .NET Core - " rel.Release.ReleaseVersion
        | ErrorPage (code, text) -> sprintf "%s: %s - " code text

    let keywords =
        [ ".NET"; ".NET Version"; "dotnet" ] @
        match page.Content with
        | HomePage _ | ErrorPage _ -> []
        | CorePage x -> 
            [ ".NET Core" ] @
            match x with
            | ChannelsOverview _ -> []
            | ChannelPage ch -> 
                [ for s in [ ".NET"; ".NET Core"; ".NET channel"; ".NET Core channel"; "dotnet"; "dotnet channel" ] -> 
                    sprintf "%s %O" s ch.ChannelVersion ]
            | ReleasePage rel -> 
                [ for s in [ ".NET"; ".NET Core"; ".NET Version"; ".NET Core Version"; "dotnet"; "dotnet version" ] -> 
                    sprintf "%s %O" s rel.Release.ReleaseVersion ]

    let description =
        match page.Content with
        | HomePage _ | ErrorPage _ -> 
            ""
        | CorePage (ChannelsOverview _) -> 
            "Channels of .NET Core. "
        | CorePage (ChannelPage ch) -> 
            sprintf "Channel %O of .NET Core, with latest release %O, latest runtime %O, latest SDK %O. " 
                ch.ChannelVersion ch.LatestRelease ch.LatestRuntime ch.LatestSdk
        | CorePage (ReleasePage rel) ->
            sprintf "Release %O of .NET Core, released on %a. " 
                rel.Release.ReleaseVersion date rel.Release.ReleaseDate

    let breadcrumbs =
        getBreadcrumbs page.Content
        |> List.map (fun (title, url) -> [ a [ _href url ] [ str title ]; span [ _class "sep" ] [ str "/" ] ])
        |> List.concat
        |> div [ _id "breadcrumbs" ]

    let channelsTable channels =
        div [ _class "table-wrapper" ] [
            table [ _class "overview-table channels-table" ] [ 
                thead [] [ tr [] [
                    th [] [ str "Channel" ]
                    th [] [ str "Support" ]
                    th [] [ str "Latest release" ]
                    th [] [ str "Latest release date" ]
                    th [] [ str "End of Life date" ]
                ] ]
                tbody [] [
                    for ch in channels -> tr [ _onclick (sprintf "location.pathname = '%s';" (channelUrl ch)) ] [ 
                        td [ _class "title" ] [ a [ _href (channelUrl ch) ] [ strf "%O" ch.ChannelVersion ] ]
                        td [ _class "support" ] [ supportIndicator ch.SupportPhase ]
                        td [ _class "latest-rel" ] [ 
                            span [ _class "label" ] [ str "Latest release: " ]
                            strf "%O" ch.LatestRelease 
                        ]
                        td [ _class "latest-rel-date" ] [ 
                            span [ _class "label" ] [ str "Last updated on " ]
                            strf "%a" date ch.LatestReleaseDate 
                        ]
                        td [ _class ("eol-date" + if ch.EolDate.IsNone then " unknown" else "") ] [ 
                            match ch.EolDate with 
                            | Some d ->
                                yield span [ _class "label" ] [ str "EOL: " ] 
                                yield strf "%a" date d 
                            | None -> 
                                yield str "-" 
                        ]
                    ]
                ]
            ]
        ]

    let content = 
        match page.Content with
        | HomePage coreInfo ->
            div [ _class "inner-container" ] [
                section [] [
                    h1 [ _class "inner-spaced" ] [ a [ _href "/core/" ] [ str ".NET Core" ] ]
                    div [ _class "inner-spaced latest-versions" ] [
                        div [] [ 
                            span [ _class "version" ] [ 
                                a [ _href coreInfo.LatestRuntimeUrl ] [ strf "%O" coreInfo.LatestRuntime ]
                            ]
                            span [ _class "label" ] [ str "Latest runtime" ]
                        ]
                        div [] [ 
                            span [ _class "version" ] [
                                a [ _href coreInfo.LatestSdkUrl ] [ strf "%O" coreInfo.LatestSdk ]
                            ]
                            span [ _class "label" ] [ str "Latest SDK" ]
                        ]
                    ]
                    h2 [ _class "inner-spaced" ] [ str "Latest channels" ]
                    channelsTable coreInfo.PrimaryChannels
                    a [ _class "inner-spaced"; _href "/core/" ] [ str "See all channels >" ]
                ]
            ]
        | CorePage (ChannelsOverview channels) -> 
            div [ _class "inner-container" ] [
                h1 [ _class "inner-spaced" ] [ str ".NET Core" ]
                channelsTable channels
            ]
        | CorePage (ChannelPage channel) ->
            div [ _class "inner-container" ] [
                h1 [ _class "inner-spaced" ] [ strf "Channel %O" channel.ChannelVersion ]
                ul [ _class "props-list" ] [
                    yield li [] [ supportIndicator channel.SupportPhase ]
                    match channel.EolDate with 
                    | Some eol -> yield li [] [ strf "End of Life on %a" date eol ]
                    | None -> ()
                    yield li [] [ a [ _href channel.LifecyclePolicy ] [ str "Lifecycle Policy" ] ]
                    yield li [] [ 
                        str "Latest runtime: " 
                        a [ _href (releaseUrl channel (getLatestRuntimeRel channel)) ] [ 
                            strf "%O" (channel.LatestRuntime |> Version.simplify) 
                        ]
                    ]
                    yield li [] [ 
                        strf "Latest SDK: " 
                        a [ _href (releaseUrl channel (getLatestSdkRel channel)) ] [ 
                            strf "%O" (channel.LatestSdk |> Version.simplify) 
                        ]
                    ]
                ]
                h2 [ _class "inner-spaced" ] [ str "Releases" ]
                div [ _class "table-wrapper" ] [
                    table [ _class "overview-table releases-table" ] [
                        thead [] [ tr [] [
                            th [] [ str "Version" ]
                            th [] [ str "Release date" ]
                            th [] [ str "Runtime" ]
                            th [] [ str "SDKs" ]
                            th [] [ str "Security" ]
                        ] ]
                        tbody [] [
                            for rel in channel.Releases -> tr [ _onclick (sprintf "location.pathname = '%s';" (releaseUrl channel rel)) ] [
                                td [ _class "title" ] [ a [ _href (releaseUrl channel rel) ] [ strf "%O" rel.ReleaseVersion ] ]
                                td [ _class "rel-date" ] [
                                    span [ _class "label" ] [ str "Released on " ]
                                    strf "%a" date rel.ReleaseDate
                                ]
                                td [ _class ("runtime" + if rel.Runtime.IsNone then " unknown" else "") ] [
                                    match rel.Runtime with
                                    | Some rt ->
                                        yield span [ _class "label" ] [ str "Runtime: " ]
                                        yield strf "%O" rt.Version
                                    | None ->
                                        yield str "-"
                                ]
                                td [ _class "sdks" ] [
                                    span [ _class "label" ] [ str "SDKs: " ]
                                    str (allSdks rel |> List.map (fun sdk -> string sdk.Version) |> String.concat ", ")
                                ]
                                td [ _class "security" ] [ if rel.Security then yield indicatorSymb [ str "!" ] "Security" "border-red" ]
                            ]
                        ]
                    ]
                ]
            ]
        | CorePage (ReleasePage releaseAndNotes) ->
            let rel = releaseAndNotes.Release
            let filesList title files =
                div [ _class "files-list" ] [
                    h3 [] [ str title ]
                    ul [] [
                        for file in files -> li [] [ a [ _href file.Url ] [ str file.Name ] ]
                    ]
                ]

            div [ _class "inner-container" ] [
                yield h1 [ _class "inner-spaced" ] [ strf "Release %O" rel.ReleaseVersion ]
                yield ul [ _class "props-list" ] [
                    yield li [] [ strf "Released on %a" date rel.ReleaseDate ]
                    match rel.Runtime with 
                    | Some rt -> yield li [] [ strf "Runtime %O" rt.Version ] 
                    | None -> ()
                    for sdk in allSdks rel -> li [] [ strf "SDK %O" sdk.Version ]
                    match rel.AspnetcoreRuntime with
                    | Some asp -> yield li [] [ strf "ASP.NET Runtime %O" asp.Version ]
                    | None -> ()
                    if rel.Security then yield li [] [ indicatorSymb [ str "!" ] "Security" "border-red" ]
                ]
                for sdk in allSdks rel do
                    let props = 
                        [ match sdk.VsVersion with 
                          | Some v -> yield li [] [ strf "Visual Studio %O" v ] 
                          | _ -> match sdk.VsSupport with Some v -> yield li [] [ str v ] | _ -> ()
                          match sdk.CsharpVersion with Some v -> yield li [] [ strf "C# %O" v ] | _ -> ()
                          match sdk.FsharpVersion with Some v -> yield li [] [ strf "F# %O" v ] | _ -> ()
                          match sdk.VbVersion with Some v -> yield li [] [ strf "VB %O" v ] | _ -> () ]
                    if not (props |> List.isEmpty) then
                        yield section [ _class "inner-spaced" ] [
                            h2 [] [ strf "SDK %O" sdk.Version ]
                            ul [ _class "props-list" ] props
                        ]
                if not (rel.CveList |> List.isEmpty) then
                    yield section [ _class "inner-spaced" ] [
                        h2 [] [ str "Security" ]
                        ul [ _class "props-list" ] [
                            for cve in rel.CveList -> li [] [ a [ _href cve.CveUrl ] [ str cve.CveId ] ]
                        ]
                    ]
                yield section [ _class "release-notes" ] [
                    match releaseAndNotes.ReleaseNotesMarkdown with
                    | Some md ->
                        yield div [ _class "header-box inner-spaced" ] [
                            h2 [] [ str "Release notes" ]
                            span [] [ str "("; a [ _href rel.ReleaseNotes.Value ] [ str "Source" ]; str ")" ]
                        ]
                        yield article [ _class "text" ] [ rawText (Markdown.ToHtml(md, mdPipeline rel.ReleaseNotes.Value)) ]
                    | None -> 
                        yield h2 [ _class "inner-spaced" ] [ str "Release notes" ]
                        match rel.ReleaseNotes with
                        | Some url -> yield p [ _class "text" ] [ 
                            str "These release notes could not be displayed. Find them here: "
                            a [ _href url ] [ str "Release notes" ] 
                          ]
                        | None -> yield p [ _class "text" ] [ 
                            strf "No release notes available for %O" rel.ReleaseVersion 
                          ]
                ]
                yield section [ _class "inner-spaced downloads" ] [
                    yield h2 [] [ str "Downloads" ]
                    yield div [ _class "files-container" ] [
                        match rel.Runtime with
                        | Some rt -> yield filesList "Runtime" rt.Files
                        | None -> ()
                        for sdk in allSdks rel ->
                            filesList (sprintf "SDK %O" sdk.Version) sdk.Files
                        match rel.AspnetcoreRuntime with
                        | Some asp -> yield filesList "ASP.NET Runtime" asp.Files
                        | None -> ()
                        match rel.Symbols with
                        | Some symb -> yield filesList "Symbols" symb.Files
                        | None -> ()
                    ]
                ]
            ]
        | ErrorPage (code, text) ->
            div [ _id "error-page" ] [
                span [ _class "status-code" ] [ str code ]
                span [ _class "status-text" ] [ str text ]
            ]

    let matomo =
        rawText """<script type="text/javascript">
  var _paq = window._paq || [];
  /* tracker methods like "setCustomDimension" should be called before "trackPageView" */
  _paq.push(["disableCookies"]);
  _paq.push(['trackPageView']);
  _paq.push(['enableLinkTracking']);
  (function() {
    var u="//analytics.arthurrump.com/";
    _paq.push(['setTrackerUrl', u+'matomo.php']);
    _paq.push(['setSiteId', '4']);
    var d=document, g=d.createElement('script'), s=d.getElementsByTagName('script')[0];
    g.type='text/javascript'; g.async=true; g.defer=true; g.src=u+'matomo.js'; s.parentNode.insertBefore(g,s);
  })();
</script>"""

    html [] [
        head [ ] [ 
            meta [ _httpEquiv "Content-Type"; _content "text/html; charset=utf-8" ]
            title [] [ strf "%s%s" titleText site.Config.Title ]
            link [ _rel "stylesheet"; _type "text/css"; _href "/style.css" ]
            meta [ _name "title"; _content titleText ]
            meta [ _name "description"; _content (description + site.Config.Description) ]
            meta [ _name "keywords"; _content (keywords |> String.concat ",") ]
            meta [ _name "copyright"; _content (sprintf "Copyright %i %s" now.Year "Arthur Rump and .NET Foundation") ]
            meta [ _name "generator"; _content "Fake.StaticGen" ]
            meta [ _name "viewport"; _content "width=device-width, initial-scale=1" ]
            link [ _rel "canonical"; _href (site.AbsoluteUrl page.Url) ]
            meta [ _property "og:url"; _content (site.AbsoluteUrl page.Url) ]
            meta [ _property "og:site_name"; _content site.Config.Title ] 
            meta [ _property "og:title"; _content titleText ]
            meta [ _property "og:type"; _content "website" ]
            meta [ _property "og:description"; _content site.Config.Description ]
            matomo
        ]
        body [ ] [ 
            header [ _id "main-header" ] [
                div [ _class "container" ] [
                    span [ _id "title" ] [ a [ _href "/" ] [ str site.Config.Title ] ]
                ]
            ]
            div [ _id "background" ] [ 
                div [ _class "container" ] [ 
                    breadcrumbs
                    content 
                ]
            ]
            footer [] [
                span [] [ a [ _href "https://github.com/arthurrump/versionsof.net" ] [ str "View on GitHub" ] ]
                span [] [ a [ _href "https://github.com/arthurrump/Fake.StaticGen" ] [ str "Generated with Fake.StaticGen" ] ]
                span [] [ a [ _href "https://arthurrump.com" ] [ str "Created by Arthur Rump" ] ]
            ]
        ] 
    ]

// Site generation
//////////////////
let createStaticSite config pages =
    StaticSite.fromConfig "https://versionsof.net" config
    |> StaticSite.withFilesFromSources (!! "icons/*" --"icons/**/ignore/**/*") Path.GetFileName
    |> StaticSite.withFilesFromSources (!! "code/*") Path.GetFileName
    |> StaticSite.withPages pages
    |> StaticSite.withPage (ErrorPage ("404", "Not Found")) "/404.html"

let tryGenerateSite config =
    async {
        match! tryGetPages config.ReleasesIndexUrl with
        | Ok pages ->
            createStaticSite config pages
            |> StaticSite.generateFromHtml "public" template
        | Error e -> failwith e
    }

// Targets
//////////
Target.create "Generate" <| fun _ ->
    let config = File.readAsString "config.toml" |> parseConfig
    tryGenerateSite config |> Async.RunSynchronously
          
Target.runOrDefault "Generate"