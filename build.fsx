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

type AsyncResultBuilder() =
    member __.Bind (x, f) = async {
        let! x = x
        match x with
        | Ok x -> return! f x
        | Error x -> return Error x
    }

    member __.Bind (x, f : 'a -> Async<Result<'b, 'c>>) = async {
        let! x = x
        return! f x
    }

    member __.Bind (x, f : 'a -> Async<Result<'b, 'c>>) = async {
        match x with
        | Ok x -> return! f x
        | Error x -> return Error x
    }

    member __.Return x = Ok x |> async.Return
    
    member __.ReturnFrom (x : Async<Result<_, _>>) = x
    // member __.ReturnFrom (x : Result<_, _>) = async.Return x
    // member __.ReturnFrom (x : Async<_>) = async { let! x = x in return Ok x }
    
    member __.Zero () = async.Return (Ok ())

    member this.TryFinally (body, fin) =
        try this.ReturnFrom (body ())
        finally fin ()

    member this.Using(disposable:#System.IDisposable, body) =
        let body' = fun () -> body disposable
        this.TryFinally(body', fun () -> 
            match disposable with 
                | null -> () 
                | disp -> disp.Dispose())

let asyncResult = AsyncResultBuilder()

let rec allOk xs =
    match xs with
    | [] -> Ok []
    | Ok x :: rest -> Result.map (fun r -> x::r) (allOk rest)
    | Error x :: _ -> Error x

// Data dowloads
////////////////
let downloadReleases (indexUrl : string) =
    asyncResult {
        use http = new HttpClient()
        let! indexJson = http.GetStringAsync indexUrl |> Async.AwaitTask
        let! index = Decode.fromString (Decode.list IndexEntry.Decoder) indexJson 
        let! releasesJson =
            index 
            |> List.map (fun i -> 
                http.GetStringAsync i.ReleasesJson 
                |> Async.AwaitTask)
            |> Async.Parallel
        let! releases = 
            releasesJson
            |> Array.toList
            |> List.map (Decode.fromString Release.Decoder)
            |> allOk
        return releases
    }

// Site generation
//////////////////
let now = DateTime.UtcNow

type Config =
    { ReleasesIndexUrl : string
      DotnetTwitter : string
      Title : string
      Description : string }

[<NoComparison>]
type Page =
    | Page
    | ErrorPage of code: string * text: string

let tomlTryGet key (toml : TomlTable) =
    if toml.ContainsKey(key) 
    then Some (toml.[key].Get())
    else None

let parseConfig config =
    let toml = Toml.ReadString(config)
    { ReleasesIndexUrl = toml.["releases-index-url"].Get()
      DotnetTwitter = toml.["dotnet-twitter"].Get()
      Title = toml.["title"].Get()
      Description = toml.["description"].Get() }

let template (site : StaticSite<Config, Page>) page = 
    let _property = XmlEngine.attr "property"

    let titleText =
        match page.Content with
        | Page -> ""
        | ErrorPage (code, text) -> sprintf "%s: %s" code text

    let pageHeader =
        header [ _id "main-header" ] [
            span [ _id "title" ] [ a [ _href "/" ] [ str "Versions of .NET Core" ] ]
        ]

    let content = 
        match page.Content with
        | Page | ErrorPage _ -> 
            div [ _class "titeled-container" ] [ ]

    let frame content =
        match page.Content with
        | Page | ErrorPage _ ->
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
        | Page ->
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

Target.create "Generate" <| fun _ ->
    StaticSite.fromConfigFile "https://versionsof.net" "config.toml" parseConfig
    |> StaticSite.withFilesFromSources (!! "icons/*") Path.GetFileName
    |> StaticSite.withFilesFromSources (!! "code/*") Path.GetFileName
    |> StaticSite.withPage (ErrorPage ("404", "Not Found")) "/404.html"
    |> StaticSite.generateFromHtml "public" template

Target.runOrDefault "Generate"