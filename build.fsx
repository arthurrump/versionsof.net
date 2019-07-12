#r "paket:
nuget Fake.Core.Target 
nuget Fake.StaticGen
nuget Fake.StaticGen.Html
nuget Fake.StaticGen.Markdown
nuget MarkdigExtensions.SyntaxHighlighting
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

open Markdig
open NetCore.Versions.Data
open Nett
open Thoth.Json.Net

open System
open System.IO
open System.Net.Http

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

let downloadGh = rewriteGithubUrls >> download
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

// Site structure and page creation
///////////////////////////////////
type Page =
    | ChannelsOverview of Channel list
    | ChannelPage of Channel
    | ReleasePage of {| Release : Release; ReleaseNotesMarkdown : string option |}
    | ErrorPage of code: string * text: string

// TODO: index.html should not be needed here, fix in Fake.StaticGen
let channelUrl ch = sprintf "/%O/index.html" ch.ChannelVersion
let releaseUrl ch rel = sprintf "/%O/%O/index.html" ch.ChannelVersion rel.ReleaseVersion

let channelsToPages channels releaseNotesMap =
    [ yield { Url = "/"; Content = ChannelsOverview channels }
      for ch in channels do
        yield { Url = channelUrl ch; Content = ChannelPage ch }
        for rel in ch.Releases do
            yield { Url = releaseUrl ch rel
                    Content = ReleasePage {| Release = rel
                                             ReleaseNotesMarkdown = releaseNotesMap |> Map.tryFind rel.ReleaseNotes |} } ]

let tryGetPages indexUrl = 
    async {
        match! tryGetChannels indexUrl with
        | Ok channels ->
            let! releaseNotesMap = tryGetReleaseNotes channels
            return Result.map (channelsToPages channels) releaseNotesMap
        | Error e -> 
            return Error e
    }

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

    let titleText =
        match page.Content with
        | ChannelsOverview _ -> "Channels"
        | ChannelPage ch -> string ch.ChannelVersion
        | ReleasePage rel -> string rel.Release.ReleaseVersion
        | ErrorPage (code, text) -> sprintf "%s: %s" code text

    let pageHeader =
        header [ _id "main-header" ] [
            span [ _id "title" ] [ a [ _href "/" ] [ str "Versions of .NET Core" ] ]
        ]

    let content = 
        match page.Content with
        | _ -> 
            div [ _class "titeled-container" ] [ ]

    let frame content =
        match page.Content with
        | _ ->
            content

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
    _paq.push(['setSiteId', '2']);
    var d=document, g=d.createElement('script'), s=d.getElementsByTagName('script')[0];
    g.type='text/javascript'; g.async=true; g.defer=true; g.src=u+'matomo.js'; s.parentNode.insertBefore(g,s);
  })();
</script>""" // TODO: correct site id

    let headerTags =
        match page.Content with
        | ChannelPage _ ->
            // [ meta [ _name "keywords"; _content (project.Tags |> String.concat ",") ]
            //   meta [ _name "title"; _content project.Title ]
            //   meta [ _name "description"; _content project.Tagline ]
            //   meta [ _name "twitter:card"; _content "summary_large_image" ]
            //   meta [ _name "twitter:creator"; _content site.Config.AuthorTwitter ]
            //   meta [ _property "og:title"; _content project.Title ]
            //   meta [ _property "og:type"; _content "website" ]
            //   meta [ _property "og:image"; _content (site.AbsoluteUrl project.Image) ]
            //   meta [ _property "og:description"; _content project.Tagline ] ]
            []
        | _ ->
            [ meta [ _name "description"; _content site.Config.Description ]
              meta [ _property "og:title"; _content titleText ]
              meta [ _property "og:type"; _content "website" ] ]

    html [] [
        head [ ] ([ 
            meta [ _httpEquiv "Content-Type"; _content "text/html; charset=utf-8" ]
            title [] [ strf "%s - %s" titleText site.Config.Title ]
            link [ _rel "stylesheet"; _type "text/css"; _href "/style.css" ]
            meta [ _name "copyright"; _content (sprintf "Copyright %i %s" now.Year "") ] // TODO
            meta [ _name "description"; _content site.Config.Description ]
            meta [ _name "generator"; _content "Fake.StaticGen" ]
            meta [ _name "viewport"; _content "width=device-width, initial-scale=1" ]
            link [ _rel "canonical"; _href (site.AbsoluteUrl page.Url) ]
            meta [ _property "og:url"; _content (site.AbsoluteUrl page.Url) ]
            meta [ _property "og:site_name"; _content site.Config.Title ] 
            meta [ _name "twitter:dnt"; _content "on" ]
            matomo
        ] @ headerTags)
        body [ ] [ 
            div [ _id "background" ] [ 
                div [ _id "container" ] [ 
                    pageHeader
                    frame content 
                ]
            ]
            footer [] [
                span [] [ a [ _href "/archives" ] [ str "Archive" ] ]
                span [] [ a [ _href "/tags" ] [ str "Tags" ] ]
                span [] [ a [ _href "/feed.xml" ] [ str "RSS Feed" ] ]
                span [] [ a [ _href "https://github.com/arthurrump/Fake.StaticGen" ] [ str "Generated with Fake.StaticGen" ] ]
                span [] [ rawText "&copy; "; strf "%i %s" now.Year "" ] // TODO
            ]
        ] 
    ]

// Site generation
//////////////////
let createStaticSite config pages =
    StaticSite.fromConfig "https://versionsof.net" config
    |> StaticSite.withFilesFromSources (!! "icons/*") Path.GetFileName
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