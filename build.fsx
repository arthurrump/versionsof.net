#r "paket:
nuget Fake.Core.Target 
nuget Fake.StaticGen
nuget Fake.StaticGen.Html
nuget Fake.StaticGen.Markdown
nuget MarkdigExtensions.SyntaxHighlighting
nuget MarkdigExtensions.UrlRewriter
nuget Nett
nuget YamlDotNet
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
open System.Text

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

module private Decode =
    let version path value =
        match Decode.string path value with
        | Ok s ->
            match Version.parse s with
            | Some v -> Ok v
            | None -> (path, BadPrimitive("a version", value)) |> Result.Error
        | Error v -> Error v

let (|DateTime|_|) text = 
    match DateTime.TryParse text with
    | (true, dt) -> Some dt
    | (false, _) -> None

// Site configuration
/////////////////////
let now = DateTime.UtcNow

type Config =
    { ReleasesIndexUrl : string
      Title : string
      Description : string

      MonoRepo : string
      MonoPath : string
      
      // Secrets, defined in secrets.toml
      GitHubClientId : string
      GitHubClientSecret : string }

let parseConfig config =
    let toml = Toml.ReadString(config)
    { ReleasesIndexUrl = toml.["releases-index-url"].Get()
      Title = toml.["title"].Get()
      Description = toml.["description"].Get()

      MonoRepo = toml.["mono-repo"].Get()
      MonoPath = toml.["mono-path"].Get()
      
      GitHubClientId = toml.["gh-client-id"].Get()
      GitHubClientSecret = toml.["gh-client-secret"].Get() }

// Data dowloads
////////////////
let download (accept : string) (url : string) =
    async {
        use http = new HttpClient()
        use req = new HttpRequestMessage(HttpMethod.Get, url)
        req.Headers.Add("Accept", accept)
        req.Headers.Add("User-Agent", "Fake.StaticGen")
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

// .NET Core - Data
/////////////////
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

// .NET Core - Data Helpers
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

// Mono - Data
//////////////
type MonoReleaseDate =
    | Released of DateTime
    | Skipped
    | Stub

let (|MonoReleaseDate|_|) = function
    | "skipped" -> Some Skipped
    | t when String.isNullOrWhiteSpace t -> Some Stub
    | DateTime dt -> Some (Released dt)
    | t -> Trace.traceImportantfn "Unparsed release date: \"%s\"" t; None

type MonoRelease =
    { Version : Version
      ReleaseDate : MonoReleaseDate
      Markdown : string
      ReleaseNotesSource : Url }

let ghFileDecoder =
    Decode.object (fun get ->
        get.Required.Field "name" Decode.string, 
        get.Required.Field "download_url" Decode.string)

let monoReleasesUrl config =
    sprintf "https://api.github.com/repos/%s/contents/%s?client_id=%s&client_secret=%s"
        config.MonoRepo config.MonoPath config.GitHubClientId config.GitHubClientSecret

let tryGetMonoReleaseUrls url = 
    async {
        let! json = getJson url
        return json 
        |> Result.bind (Decode.fromString (Decode.list ghFileDecoder))
        |> Result.map (List.filter (fun (file, _) -> file <> "index.md") >> List.map snd)
    }

let yamlDecode = 
    let deserializer = YamlDotNet.Serialization.Deserializer()
    fun (text : string) ->
        deserializer.Deserialize<Collections.Generic.Dictionary<string, string>>(text)

let tryParseMonoRelease sourceUrl text =
    match Markdown.splitFrontmatter text with
    | (Some yaml, md) ->
        let fields = yamlDecode yaml
        match fields.["version"], fields.["releasedate"] with
        | Version.Version v, MonoReleaseDate d ->
            Ok { Version = v; ReleaseDate = d; Markdown = md; ReleaseNotesSource = sourceUrl }
        | _ -> 
            Error (sprintf "Couldn't parse Mono release %s" sourceUrl)
    | _ -> 
        Error (sprintf "No frontmatter found in Mono release %s" sourceUrl)

let tryGetMonoReleasesFromUrls urls =
    async {
        let! files =
            urls
            |> List.map (fun url ->
                downloadGh "text/plain" url
                |> Async.map (Result.bind (tryParseMonoRelease url)))
            |> Async.Parallel
        return files |> List.ofArray |> Result.allOk
    }

let tryGetMonoReleases config =
    async {
        let! urls = tryGetMonoReleaseUrls (monoReleasesUrl config)
        match urls with
        | Ok urls -> return! tryGetMonoReleasesFromUrls urls
        | Error e -> return Error e
    }

// .NET Core - Query data sources
/////////////////////////////////
module Query =
    type Sdk =
        { Version : Version
          ReleaseLink : string // channel/release
          ReleaseDate : DateTime
          RuntimeVersion : Version option
          VsVersion : Version option
          CsharpVersion : Version option
          FsharpVersion : Version option
          VbVersion : Version option }

        static member Encoder sdk =
            Encode.object [
                yield "version", Encode.string (string sdk.Version)
                yield "release", Encode.string sdk.ReleaseLink
                yield "date", Encode.datetime sdk.ReleaseDate
                match sdk.RuntimeVersion with Some rt -> yield "runtime", Encode.string (string rt) | _ -> ()
                match sdk.VsVersion with Some rt -> yield "vs", Encode.string (string rt) | _ -> ()
                match sdk.CsharpVersion with Some rt -> yield "csharp", Encode.string (string rt) | _ -> ()
                match sdk.FsharpVersion with Some rt -> yield "fsharp", Encode.string (string rt) | _ -> ()
                match sdk.VbVersion with Some rt -> yield "vb", Encode.string (string rt) | _ -> ()
            ]

        static member Decoder =
            Decode.object (fun get ->
                { Version = get.Required.Field "version" Decode.version
                  ReleaseLink = get.Required.Field "release" Decode.string
                  ReleaseDate = get.Required.Field "date" Decode.datetime
                  RuntimeVersion = get.Optional.Field "runtime" Decode.version
                  VsVersion = get.Optional.Field "vs" Decode.version
                  CsharpVersion = get.Optional.Field "csharp" Decode.version
                  FsharpVersion = get.Optional.Field "fsharp" Decode.version
                  VbVersion = get.Optional.Field "vb" Decode.version })

    type Runtime =
        { Version : Version
          ReleaseLink : string // channel/release
          ReleaseDate : DateTime
          VsVersion : Version list }

        static member Encoder rt =
            Encode.object [
                "version", Encode.string (string rt.Version)
                "release", Encode.string rt.ReleaseLink
                "date", Encode.datetime rt.ReleaseDate
                "vs", Encode.list (rt.VsVersion |> List.map (string >> Encode.string))
            ]

        static member Decoder =
            Decode.object (fun get ->
                { Version = get.Required.Field "version" Decode.version
                  ReleaseLink = get.Required.Field "release" Decode.string
                  ReleaseDate = get.Required.Field "date" Decode.datetime
                  VsVersion = get.Required.Field "vs" (Decode.list Decode.version) })

    type Release =
        { Version : Version
          ReleaseDate : DateTime
          Runtime : Version option
          Sdks : Version list
          AspRuntime : Version option
          Cves : string list }

        static member Encoder rel =
            Encode.object [
                yield "version", Encode.string (string rel.Version)
                yield "date", Encode.datetime rel.ReleaseDate
                match rel.Runtime with Some rt -> yield "runtime", Encode.string (string rt) | _ -> ()
                yield "sdks", Encode.list (rel.Sdks |> List.map (string >> Encode.string))
                match rel.AspRuntime with Some asp -> yield "asp", Encode.string (string asp) | _ -> ()
                yield "cves", Encode.list (rel.Cves |> List.map Encode.string)
            ]

        static member Decoder =
            Decode.object (fun get ->
                { Version = get.Required.Field "version" Decode.version
                  ReleaseDate = get.Required.Field "date" Decode.datetime
                  Runtime = get.Optional.Field "runtime" Decode.version
                  Sdks = get.Required.Field "sdks" (Decode.list Decode.version)
                  AspRuntime = get.Optional.Field "asp" Decode.version
                  Cves = get.Required.Field "cves" (Decode.list Decode.string) })

    let getSdks channels =
        [ for ch in channels do 
            for rel in ch.Releases do
                for sdk in allSdks rel ->
                    { Version = sdk.Version
                      ReleaseLink = sprintf "%O/%O" ch.ChannelVersion rel.ReleaseVersion
                      ReleaseDate = rel.ReleaseDate
                      RuntimeVersion = sdk.RuntimeVersion
                      VsVersion = sdk.VsVersion
                      CsharpVersion = sdk.CsharpVersion
                      FsharpVersion = sdk.FsharpVersion
                      VbVersion = sdk.VbVersion } ]
        |> List.sortByDescending (fun sdk -> sdk.ReleaseDate)

    let getRuntimes channels =
        [ for ch in channels do
            for rel in ch.Releases do
                match rel.Runtime with
                | None -> ()
                | Some rt -> 
                    yield { Version = rt.Version
                            ReleaseLink = sprintf "%O/%O" ch.ChannelVersion rel.ReleaseVersion
                            ReleaseDate = rel.ReleaseDate
                            VsVersion = rt.VsVersion } ]
        |> List.sortByDescending (fun rt -> rt.ReleaseDate)

    let getReleases channels =
        channels
        |> List.map (fun ch -> ch.Releases)
        |> List.concat
        |> List.map (fun rel ->
            { Version = rel.ReleaseVersion
              ReleaseDate = rel.ReleaseDate
              Runtime = rel.Runtime |> Option.map (fun rt -> rt.Version)
              Sdks = allSdks rel |> List.map (fun sdk -> sdk.Version)
              AspRuntime = rel.AspnetcoreRuntime |> Option.map (fun asp -> asp.Version)
              Cves = rel.CveList |> List.map (fun cve -> cve.CveId) })
        |> List.sortByDescending (fun rel -> rel.ReleaseDate)

// Query data sources
/////////////////////
let getQueryDataFiles channels =
    let jsonFile path encoder objects =
        { Url = path
          Content = List.map encoder objects |> Encode.list |> Encode.toString 0 |> Encoding.UTF8.GetBytes }
    [ channels |> Query.getSdks |> jsonFile "/query/core/sdks.json"  Query.Sdk.Encoder   
      channels |> Query.getRuntimes |> jsonFile "/query/core/runtimes.json"  Query.Runtime.Encoder  
      channels |> Query.getReleases |> jsonFile "/query/core/releases.json"  Query.Release.Encoder ]

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

type MonoInfo =
    { LatestRelease : MonoRelease
      ReleasesSubset : MonoRelease list }

type MonoPage =
    | ReleasesOverview of MonoRelease list
    | ReleasePage of MonoRelease

type Page =
    | HomePage of CoreInfo * MonoInfo
    | CorePage of CorePage
    | MonoPage of MonoPage
    | ErrorPage of code: string * text: string

let coreChannelUrl ch = sprintf "/core/%O/" ch.ChannelVersion
let coreReleaseUrl ch rel = sprintf "/core/%O/%O/" ch.ChannelVersion rel.ReleaseVersion

let channelsToPages channels releaseNotesMap =
    [ yield { Url = "/core/"; Content = ChannelsOverview channels }
      for ch in channels do
        yield { Url = coreChannelUrl ch; Content = ChannelPage ch }
        for rel in ch.Releases do
            yield { Url = coreReleaseUrl ch rel
                    Content = CorePage.ReleasePage
                        {| Channel = ch
                           Release = rel
                           ReleaseNotesMarkdown = releaseNotesMap |> Map.tryFind rel.ReleaseNotes |} } ]
    |> List.map (fun cp -> { Url = cp.Url; Content = CorePage cp.Content })

let monoReleaseUrl rel = sprintf "/mono/%O/" rel.Version

let monoReleasesToPages releases =
    [ yield { Url = "/mono/"; Content = ReleasesOverview releases }
      for rel in releases -> { Url = monoReleaseUrl rel; Content = MonoPage.ReleasePage rel } ]
    |> List.map (fun mp -> { Url = mp.Url; Content = MonoPage mp.Content })

let getHomePage channels monoReleases =
    let coreInfo =
        let current = channels |> List.find (fun ch -> ch.SupportPhase = "current")
        { LatestRuntime = current.LatestRuntime
          LatestRuntimeUrl = coreReleaseUrl current (getLatestRuntimeRel current)
          LatestSdk = current.LatestSdk
          LatestSdkUrl = coreReleaseUrl current (getLatestSdkRel current)
          PrimaryChannels = channels |> List.filter (fun ch -> ch.SupportPhase = "current" || ch.SupportPhase = "lts") }
    let monoInfo =
        let released =
            monoReleases 
            |> List.filter (fun rel -> match rel.ReleaseDate with Released _ -> true | _ -> false)
            |> List.sortByDescending (fun rel -> rel.Version)
        { LatestRelease = released |> List.head
          ReleasesSubset = released |> List.take 3 }
    { Url = "/"; Content = HomePage (coreInfo, monoInfo) }

let tryGetPagesAndFiles config = 
    async {
        match! tryGetChannels config.ReleasesIndexUrl with
        | Ok channels ->
            match! tryGetMonoReleases config with
            | Ok monoReleases ->
                let! releaseNotesMap = tryGetReleaseNotes channels
                let corePages = Result.map (channelsToPages channels) releaseNotesMap
                let monoPages = monoReleasesToPages monoReleases
                let homePage = getHomePage channels monoReleases
                let queryJson = getQueryDataFiles channels
                return Result.map (fun cps -> homePage::cps @ monoPages, queryJson) corePages
            | Error e ->
                return Error e
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
        | CorePage.ReleasePage rel -> [ ov; (sprintf "Channel %O" rel.Channel.ChannelVersion, coreChannelUrl rel.Channel) ]
    | MonoPage x ->
        home ::
        match x with
        | ReleasesOverview _ -> []
        | ReleasePage _ -> [ ("Mono", "/mono/") ]
    | ErrorPage _ -> []

// Template
///////////
let template (site : StaticSite<Config, Page>) page = 
    let _property = XmlEngine.attr "property"
    let date () (d : DateTime) = d.ToLocalTime().ToString "yyyy-MM-dd" 

    let mdPipeline (file : string) =
        MarkdownPipelineBuilder()
            .UsePipeTables()
            .UseAutoLinks()
            .UseAutoIdentifiers(Extensions.AutoIdentifiers.AutoIdentifierOptions.GitHub)
            .UseSyntaxHighlighting()
            .UseUrlRewriter(fun link -> 
                if Uri.IsWellFormedUriString(link.Url, UriKind.Absolute) || link.Url.StartsWith("#") then
                    link.Url
                else if link.Url.StartsWith("/docs/about-mono/releases/") then
                    link.Url.Replace("/docs/about-mono/releases", "/mono")
                else if file.Contains("/docs/about-mono/releases/") then
                    "https://www.mono-project.com" + link.Url
                else
                    file.Substring(0, file.LastIndexOf('/')) + "/" + link.Url)
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
        | CorePage (CorePage.ReleasePage rel) -> sprintf ".NET Core %O - " rel.Release.ReleaseVersion
        | MonoPage (ReleasesOverview _) -> "Mono - "
        | MonoPage (ReleasePage rel) -> sprintf "Mono %O - " rel.Version
        | ErrorPage (code, text) -> sprintf "%s: %s - " code text

    let keywords =
        [ ".NET"; ".NET Version"; "dotnet"; ".NET Release"; "dotnet release" ] @
        match page.Content with
        | HomePage _ | ErrorPage _ -> []
        | CorePage x -> 
            [ ".NET Core" ] @
            match x with
            | ChannelsOverview _ -> []
            | ChannelPage ch -> 
                [ for s in [ ".NET"; ".NET Core"; ".NET channel"; ".NET Core channel"; "dotnet"; "dotnet channel" ] -> 
                    sprintf "%s %O" s ch.ChannelVersion ]
            | CorePage.ReleasePage rel -> 
                [ for s in [ ".NET"; ".NET Core"; ".NET Version"; ".NET Core Version"; "dotnet"; "dotnet version"
                             ".NET Core Release"; "dotnet release" ] -> 
                    sprintf "%s %O" s rel.Release.ReleaseVersion ]
        | MonoPage x ->
            [ "Mono"; "Mono Version" ] @
            match x with
            | ReleasesOverview _ -> []
            | ReleasePage rel -> 
                [ for s in [ "Mono"; "Mono Version"; "Mono Release" ] -> 
                    sprintf "%s %O" s rel.Version ]

    let description =
        match page.Content with
        | HomePage _ | ErrorPage _ -> 
            ""
        | CorePage (ChannelsOverview _) -> 
            "Channels of .NET Core. "
        | CorePage (ChannelPage ch) -> 
            sprintf "Channel %O of .NET Core, with latest release %O, latest runtime %O, latest SDK %O. " 
                ch.ChannelVersion ch.LatestRelease ch.LatestRuntime ch.LatestSdk
        | CorePage (CorePage.ReleasePage rel) ->
            sprintf "Release %O of .NET Core, released on %a. " 
                rel.Release.ReleaseVersion date rel.Release.ReleaseDate
        | MonoPage (ReleasesOverview _) ->
            "Releases of Mono. "
        | MonoPage (ReleasePage rel) ->
            match rel.ReleaseDate with
            | Released relDate ->
                sprintf "Release %O of Mono, released on %a. " rel.Version date relDate
            | Skipped ->
                sprintf "Skipped release %O of Mono. " rel.Version
            | Stub ->
                sprintf "Release %O of Mono. " rel.Version

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
                    for ch in channels -> tr [ _onclick (sprintf "location.pathname = '%s';" (coreChannelUrl ch)) ] [ 
                        td [ _class "title" ] [ a [ _href (coreChannelUrl ch) ] [ strf "%O" ch.ChannelVersion ] ]
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

    let monoReleaseTable releases =
        div [ _class "table-wrapper" ] [
            table [ _class "overview-table mono-table" ] [
                thead [] [ tr [] [
                    th [] [ str "Version" ]
                    th [] [ str "Release date" ]
                ] ]
                tbody [] [
                    for rel in releases -> tr [ _onclick (sprintf "location.pathname = '%s';" (monoReleaseUrl rel)) ] [
                        td [ _class "title" ] [ a [ _href (monoReleaseUrl rel) ] [ strf "%O" rel.Version ] ]
                        td [ _class "rel-date" ] [
                            match rel.ReleaseDate with
                            | Released relDate ->
                                yield span [ _class "label" ] [ str "Released on " ]
                                yield strf "%a" date relDate
                            | Skipped ->
                                yield str "Skipped"
                            | Stub ->
                                yield str "Preview"
                        ]
                    ]
                ]
            ]
        ]

    let content = 
        match page.Content with
        | HomePage (coreInfo, monoInfo) ->
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
                    h2 [ _class "inner-spaced" ] [ str "Supported channels" ]
                    channelsTable coreInfo.PrimaryChannels
                    a [ _class "inner-spaced"; _href "/core/" ] [ str "See all channels >" ]
                ]
                section [] [
                    h1 [ _class "inner-spaced" ] [ a [ _href "/mono/" ] [ str "Mono" ] ]
                    div [ _class "inner-spaced latest-versions" ] [
                        div [] [
                            span [ _class "version" ] [
                                a [ _href (monoReleaseUrl monoInfo.LatestRelease) ] 
                                  [ strf "%O" monoInfo.LatestRelease.Version ]
                            ]
                            span [ _class "label" ] [ str "Latest release" ]
                        ]
                    ]
                    h2 [ _class "inner-spaced" ] [ str "Recent releases" ]
                    monoReleaseTable monoInfo.ReleasesSubset
                    a [_class "inner-spaced"; _href "/mono/" ] [ str "See all releases >" ]
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
                        a [ _href (coreReleaseUrl channel (getLatestRuntimeRel channel)) ] [ 
                            strf "%O" (channel.LatestRuntime |> Version.simplify) 
                        ]
                    ]
                    yield li [] [ 
                        strf "Latest SDK: " 
                        a [ _href (coreReleaseUrl channel (getLatestSdkRel channel)) ] [ 
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
                            for rel in channel.Releases -> tr [ _onclick (sprintf "location.pathname = '%s';" (coreReleaseUrl channel rel)) ] [
                                td [ _class "title" ] [ a [ _href (coreReleaseUrl channel rel) ] [ strf "%O" rel.ReleaseVersion ] ]
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
        | CorePage (CorePage.ReleasePage releaseAndNotes) ->
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
        | MonoPage (ReleasesOverview releases) ->
            let releases = releases |> List.sortByDescending (fun rel -> rel.Version)
            div [ _class "inner-container" ] [
                h1 [ _class "inner-spaced" ] [ str "Mono" ]
                monoReleaseTable releases
            ]
        | MonoPage (ReleasePage rel) ->
            div [ _class "inner-container" ] [
                h1 [ _class "inner-spaced" ] [ strf "Release %O" rel.Version ]
                ul [ _class "props-list" ] [
                    li [] [ 
                        match rel.ReleaseDate with
                        | Released relDate -> yield strf "Released on %a" date relDate
                        | Skipped -> yield str "Skipped"
                        | Stub -> yield str "Preview"
                    ]
                ]
                section [ _class "release-notes" ] [
                    div [ _class "header-box inner-spaced" ] [
                        h2 [] [ str "Release notes" ]
                        span [] [ str "("; a [ _href rel.ReleaseNotesSource ] [ str "Source" ]; str ")" ]
                    ]
                    article [ _class "text" ] [ 
                        rawText (Markdown.ToHtml(rel.Markdown, mdPipeline rel.ReleaseNotesSource))
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

    html [ _lang "en" ] [
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
            meta [ _property "og:title"; _content (titleText + site.Config.Title) ]
            meta [ _property "og:type"; _content "website" ]
            meta [ _property "og:description"; _content (description + site.Config.Description) ]
            meta [ _property "og:image"; _content "/logo.png" ]
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
let createStaticSite config pages files =
    StaticSite.fromConfig "https://versionsof.net" config
    |> StaticSite.withFilesFromSources (!! "icons/*" --"icons/**/ignore/**/*") Path.GetFileName
    |> StaticSite.withFilesFromSources (!! "code/*") Path.GetFileName
    |> StaticSite.withFilesFromSources (!! "rootfiles/*") Path.GetFileName
    |> StaticSite.withPages pages
    |> StaticSite.withFiles files
    |> StaticSite.withPage (ErrorPage ("404", "Not Found")) "/404.html"

let generateSite config =
    async {
        match! tryGetPagesAndFiles config with
        | Ok (pages, files) ->
            createStaticSite config pages files
            |> StaticSite.generateFromHtml "public" template
        | Error e -> failwith e
    }

// Targets
//////////
let [<Literal>] configFile = "config.toml"
let [<Literal>] secretsFile = "secrets.toml"

Target.create "Generate" <| fun _ ->
    if File.exists secretsFile then
        let config = 
            [ configFile; secretsFile ]
            |> List.map File.readAsString
            |> String.concat "\n" 
            |> parseConfig
        generateSite config |> Async.RunSynchronously
    else
        Trace.traceErrorfn "Could not find file '%s'. Please create one using the Configure target." secretsFile

Target.create "Configure" <| fun p ->
    match p.Context.Arguments with
    | [ ghClientId; ghClientSecret ] ->
        let toml = Toml.Create()
        toml.Add("gh-client-id", ghClientId) |> ignore
        toml.Add("gh-client-secret", ghClientSecret) |> ignore
        Toml.WriteFile(toml, secretsFile)
    | _ ->
        Trace.traceErrorfn "Invalid arguments for Configure"
        Trace.traceErrorfn "Required arguments: [GitHub Client ID] [GitHub Client Secret]"
          
Target.runOrDefaultWithArguments "Generate"