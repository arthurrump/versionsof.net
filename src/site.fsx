module Site

#load "../.fake/build.fsx/intellisense.fsx"
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
open Fake.StaticGen.Html.ViewEngine.Accessibility
open Fake.StaticGen.Markdown

open System.IO
open System.Text

open Markdig

#load "helpers.fsx"
open Helpers

#load "core.fsx"
#load "mono.fsx"
#load "query.fsx"

// Site structure and page creation
///////////////////////////////////
type Page =
    | HomePage of Core.Info * Mono.Info
    | CorePage of Core.Page
    | MonoPage of Mono.Page
    | ErrorPage of code: string * text: string

let getHomePage channels monoReleases =
    { Url = "/"; Content = HomePage (Core.getInfo channels, Mono.getInfo monoReleases) }

let tryGetPagesAndFiles config = 
    async {
        match! Core.tryGetChannels config.ReleasesIndexUrl with
        | Ok channels ->
            match! Mono.tryGetReleases config with
            | Ok monoReleases ->
                let! releaseNotesMap = Core.tryGetReleaseNotes channels
                let corePages = Result.map (Core.channelsToPages channels >> Page.mapl CorePage) releaseNotesMap
                let monoPages = Mono.releasesToPages monoReleases |> Page.mapl MonoPage
                let homePage = getHomePage channels monoReleases
                let queryJson = Query.getDataFiles channels
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
    | CorePage p -> home :: Core.getBreadcrumbs p
    | MonoPage p -> home :: Mono.getBreadcrumbs p
    | ErrorPage _ -> []

// Template
///////////
let template (site : StaticSite<Config, Page>) page = 
    let _property = XmlEngine.attr "property"

    let titleText =
        match page.Content with
        | HomePage _ -> ""
        | CorePage p -> Core.titleText p
        | MonoPage p -> Mono.titleText p
        | ErrorPage (code, text) -> sprintf "%s: %s - " code text

    let keywords =
        [ ".NET"; ".NET Version"; "dotnet"; ".NET Release"; "dotnet release" ] @
        match page.Content with
        | HomePage _ | ErrorPage _ -> []
        | CorePage p -> Core.keywords p
        | MonoPage p -> Mono.keywords p

    let description =
        match page.Content with
        | HomePage _ | ErrorPage _ -> ""
        | CorePage p -> Core.description p
        | MonoPage p -> Mono.description p
    
    let navItem name url =
        span [ _class "nav-item" ] [ a [ _href url ] [ str name ] ]

    let breadcrumbs =
        getBreadcrumbs page.Content
        |> List.map (fun (title, url) -> [ a [ _href url ] [ str title ]; span [ _class "sep" ] [ str "/" ] ])
        |> List.concat
        |> div [ _id "breadcrumbs" ]

    let content = 
        match page.Content with
        | HomePage (coreInfo, monoInfo) ->
            div [ _class "inner-container" ] [
                Core.homeSection coreInfo
                Mono.homeSection monoInfo
            ]
        | CorePage p -> Core.content p
        | MonoPage p -> Mono.content p
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

    let iconList = set [ "md-close"; "md-menu" ]
    let icon name = 
        XmlEngine.tag "svg" [ _class "icon"; _ariaHidden "true" ] [ 
            XmlEngine.tag "use" [ XmlEngine.attr "xlink:href" ("#icon-" + name) ] [] ]

    let iconsCombined =
        XmlEngine.tag "svg" [
            XmlEngine.attr "xmlns" "http://www.w3.org/2000/svg"
            _style "display: none;" 
        ] (iconList 
           |> Set.map (fun icon -> 
                site.Files 
                |> Seq.find (fun p -> p.Url |> Path.GetFileNameWithoutExtension = icon) 
                |> fun p -> p.Content 
                |> Encoding.UTF8.GetString
                |> String.replace "xmlns=\"http://www.w3.org/2000/svg\"" ""
                |> String.replace "<svg" ("<symbol id=\"icon-" + icon + "\"")
                |> String.replace "</svg" "</symbol"
                |> rawText)
           |> Set.toList)

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
            iconsCombined 
            header [ _id "main-header" ] [
                div [ _class "container" ] [
                    span [ _id "title" ] [ a [ _href "/" ] [ str site.Config.Title ] ]
                    button [ _id "menu-toggle"
                             _roleButton
                             _ariaLabel "Menu"
                             _onclick ("var img = document.querySelector('#menu-toggle > svg > use');"
                                + "var nav = document.getElementById('main-nav');"
                                + "if (nav.classList.contains('opened')) {"
                                + "  nav.classList.remove('opened'); img.setAttribute('xlink:href', '#icon-md-menu'); }"
                                + "else { nav.classList.add('opened'); img.setAttribute('xlink:href', '#icon-md-close'); }") ] [
                        icon "md-menu"
                    ]
                    nav [ _id "main-nav" ] [
                        navItem ".NET Core" "/core/"
                        navItem "Mono" "/mono/"
                    ]
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
    |> StaticSite.withFilesFromSources (!! "css/*") Path.GetFileName
    |> StaticSite.withFilesFromSources (!! "rootfiles/*") Path.GetFileName
    |> StaticSite.withFilesFromSources (!! "ionicons/*") (fun p -> "/icons/" + Path.GetFileName p)
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