module Framework

#load "../.fake/build.fsx/intellisense.fsx"
#if !FAKE
    #r "Facades/netstandard" // Intellisense fix, see FAKE #1938
    #r "netstandard"
#endif

#load "helpers.fsx"
open Helpers

open Fake.Core
open Fake.StaticGen
open Fake.StaticGen.Html.ViewEngine

open System
open System.Text.RegularExpressions

open FSharp.Data
open FSharpPlus.Builders
open FSharpPlus.Data
open Markdig
open NetCore.Versions
open NetCore.Versions.Data

// Data
///////
type ReleaseNotes =
    { Link : Url
      Markdown : string option }

type Release =
    { Version : Version
      ReleaseDate : DateTime
      ClrVersion : Version
      IncludedInWindows : string option
      IncludedInServer : string option
      InstallableOnWindows : string list
      InstallableOnServer : string list
      ReleaseNotes : ReleaseNotes option
      NewFeaturesLink : Url option
      NewAccessibilityLink : Url option }

let [<Literal>] wikipediaUrl = "https://en.wikipedia.org/wiki/Template:.NET_Framework_version_history"
let [<Literal>] msDocsUrl = "https://docs.microsoft.com/en-us/dotnet/framework/migration-guide/versions-and-dependencies"

type Wikipedia = HtmlProvider<wikipediaUrl>
type MsDocs = HtmlProvider<msDocsUrl, IncludeLayoutTables = true>

let wikiString =
    let regex = new Regex(@"\[[^\]]*\]")
    fun (str : string) ->
        if str.Contains "N/A" 
        then None 
        else Some (regex.Replace(str, String.Empty))

let wikiParse tryParse err str =
    match str |> wikiString |> Option.bind tryParse with
    | Some res -> Ok res
    | None -> Error (err str)

let wikiVersion = wikiParse Version.parse (sprintf "Couldn't parse '%s' as a version")
let wikiDate = wikiParse DateTime.tryParse (sprintf "Couldn't parse '%s' as a date")

let wikiList = 
    wikiString 
    >> Option.map (fun s -> 
        s.Split([| ',' |], StringSplitOptions.RemoveEmptyEntries)
        |> List.ofArray
        |> List.map String.trim)
    >> Option.defaultValue []

let docsLinkToUrl (docUrl : string) (link : HtmlNode) =
    let rec combineUrl (baseUrl : string) (suffix : string) =
        let baseUrl = baseUrl.TrimEnd('/').Substring(0, baseUrl.LastIndexOf '/')
        if suffix.StartsWith("../") 
        then combineUrl baseUrl (suffix.Substring(3))
        else baseUrl + "/" + suffix

    monad {
        let! href = link.TryGetAttribute "href"
        let url = href.Value()
        let! linkType = link.TryGetAttribute "data-linktype"
        match linkType.Value() with
        | "relative-path" -> 
            return combineUrl docUrl url
        | "external" -> 
            return url
        | other ->
            Trace.traceImportantfn "Framework: Unknown linktype attribute '%s'" other
            return! None
    }

let findLinkUrl text : HtmlNode list -> string option =
    List.tryFind (fun a -> a.DirectInnerText().Trim() = text)
    >> Option.bind (docsLinkToUrl msDocsUrl)

let getReleaseNotesMarkdown (urlOption : string option) =
    match urlOption with
    | Some url when url.EndsWith ".md" -> 
        downloadGh "plain/text" url |> Async.map (Result.map Some)
    | Some _ | None -> 
        async { return Ok None }

let rowToRelease (wikiRow : Wikipedia.OverviewOfNetFrameworkReleaseHistory1234.Row) (docsRow : HtmlNode) =
    monad {
        let! version = async { return wikiRow.Version |> wikiVersion } |> ResultT
        let! date = async { return wikiRow.``Release date`` |> wikiDate } |> ResultT
        let! clr = async { return wikiRow.CLR |> string |> wikiVersion } |> ResultT
        let windows = wikiRow.``Included in - Windows`` |> wikiString
        let server = wikiRow.``Included in - Windows Server`` |> wikiString
        let installWindows = wikiRow.``Can be installed on[2] - Windows`` |> wikiList
        let installServer = wikiRow.``Can be installed on[2] - Windows Server`` |> wikiList

        let links = docsRow.CssSelect("td").Head.CssSelect("a")
        let features = links |> findLinkUrl "New features"
        let accessibility = links |> findLinkUrl "New in accessibility"

        let releaseNotes = links |> findLinkUrl "Release notes"
        let! releaseNotesMarkdown = getReleaseNotesMarkdown releaseNotes |> ResultT
        
        return 
            { Version = version
              ReleaseDate = date
              ClrVersion = clr
              IncludedInWindows = windows
              IncludedInServer = server
              InstallableOnWindows = installWindows
              InstallableOnServer = installServer
              ReleaseNotes = releaseNotes |> Option.map (fun url -> { Link = url; Markdown = releaseNotesMarkdown })
              NewFeaturesLink = features
              NewAccessibilityLink = accessibility }
    } |> ResultT.run

let getVersion (docRow : HtmlNode) =
    docRow.CssSelect("td").Head.DirectInnerText().Split('\n').[0].Trim()

let tryGetReleases () =
    let nv = Version.parse >> Option.map (Version.pad 3)
    async {
        let! wiki = Wikipedia.AsyncGetSample()
        let! msDocs = MsDocs.AsyncGetSample()
        let docsTable = msDocs.Html.CssSelect("main#main table") |> List.head
        let docsRows = docsTable.CssSelect("tbody tr")
        return!
            wiki.Tables.``Overview of .NET Framework release history[1][2][3][4]``.Rows
            |> List.ofArray
            |> List.choose (fun row ->  
                docsRows 
                |> List.tryFind (fun r -> nv (getVersion r) = nv row.Version)
                |> Option.map (rowToRelease row)
                |> Option.map (Async.map (Result.mapError (sprintf "Error parsing Framework %A: %s" row.Version))))
            |> Async.Parallel
            |> Async.map (Array.toList >> Result.allOk)
    }

// Pages
////////
type Info =
    { LatestRelease : Release
      ReleasesSubset : Release list }

type Page =
    | ReleasesOverview of Release list * Info
    | ReleasePage of Release

let releaseUrl rel = sprintf "/framework/%O/" rel.Version

let getInfo releases =
    let releases = releases |> List.sortByDescending (fun rel -> rel.Version)
    { LatestRelease = releases |> List.head
      ReleasesSubset = releases |> List.take 3 }

let releasesToPages releases =
    [ yield { Url = "/framework/"; Content = ReleasesOverview (releases, getInfo releases) }
      for rel in releases -> { Url = releaseUrl rel; Content = ReleasePage rel } ]

let getBreadcrumbs = function
    | ReleasesOverview _ -> []
    | ReleasePage _ -> [ (".NET Framework", "/framework/") ]

// Template
///////////
let titleText = function
    | ReleasesOverview _ -> ".NET Framework - "
    | ReleasePage rel -> sprintf ".NET Framework %O - " rel.Version

let keywords page =
    [ ".NET Framework"; ".NET Framework Version"; "Full .NET" ] @
    match page with
    | ReleasesOverview _ -> []
    | ReleasePage rel -> 
        [ for s in [ ".NET Framework"; ".NET Framework Version"; ".NET Framework Release" ] -> 
            sprintf "%s %O" s rel.Version ]

let description = function
    | ReleasesOverview _ ->
        "Releases of .NET Framework. "
    | ReleasePage rel ->
        sprintf "Release %O of the .NET Framework, released on %a. " rel.Version date rel.ReleaseDate

let private releasesTable releases =
    div [ _class "table-wrapper" ] [
        table [ _class "overview-table framework-table" ] [
            thead [] [ tr [] [
                th [] [ str "Version" ]
                th [] [ str "Release date" ]
                th [] [ str "CLR Version" ]
                th [] [ str "Included in Windows" ]
                th [] [ str "Included in Windows Server" ]
            ] ]
            tbody [] [
                for rel in releases -> tr [ _onclick (sprintf "location.pathname = '%s';" (releaseUrl rel)) ] [
                    td [ _class "title" ] [ a [ _href (releaseUrl rel) ] [ strf "%O" rel.Version ] ]
                    td [ _class "rel-date" ] [
                        yield span [ _class "label" ] [ str "Released on " ]
                        yield strf "%a" date rel.ReleaseDate
                    ]
                    td [ _class "clr" ] [
                        span [ _class "label" ] [ str "CLR Version: " ]
                        strf "%O" rel.ClrVersion
                    ]
                    td [ _class ("windows" + if rel.IncludedInWindows.IsNone then " unknown" else "") ] [
                        match rel.IncludedInWindows with
                        | Some windows ->
                            yield span [ _class "label" ] [ str "Included in Windows " ]
                            yield str windows
                        | None ->
                            yield str "-"
                    ]
                    td [ _class ("server" + if rel.IncludedInServer.IsNone then " unknown" else "") ] [
                        match rel.IncludedInServer with
                        | Some server ->
                            yield span [ _class "label" ] [ str "Included in Windows Server " ]
                            yield str server
                        | None ->
                            yield str "-"
                    ]
                ]
            ]
        ]
    ]

let private header info =
    div [ _class "inner-spaced latest-versions" ] [
        div [] [
            span [ _class "version" ] [
                a [ _href (releaseUrl info.LatestRelease) ] 
                  [ strf "%O" info.LatestRelease.Version ]
            ]
            span [ _class "label" ] [ str "Latest release" ]
        ]
    ]

let homeSection info =
    section [] [
        h1 [ _class "inner-spaced" ] [ a [ _href "/framework/" ] [ str ".NET Framework" ] ]
        header info
        h2 [ _class "inner-spaced" ] [ str "Recent releases" ]
        releasesTable info.ReleasesSubset
        a [_class "inner-spaced"; _href "/framework/" ] [ str "See all releases >" ]
    ]

let content = function
    | ReleasesOverview (releases, monoInfo) ->
        let releases = releases |> List.sortByDescending (fun rel -> rel.Version)
        div [ _class "inner-container" ] [
            h1 [ _class "inner-spaced" ] [ str ".NET Framework" ]
            header monoInfo
            h2 [ _class "inner-spaced" ] [ str "Releases" ]
            releasesTable releases
        ]
    | ReleasePage rel ->
        div [ _class "inner-container" ] [
            h1 [ _class "inner-spaced" ] [ strf "Release %O" rel.Version ]
            ul [ _class "props-list" ] [
                yield li [] [ strf "Released on %a" date rel.ReleaseDate ]
                yield li [] [ strf "CLR Version %O" rel.ClrVersion ]
                match rel.IncludedInWindows with
                | Some windows -> yield li [] [ strf "Included in Windows %s" windows ]
                | None -> ()
                match rel.InstallableOnWindows with
                | [] -> ()
                | ws -> yield li [] [ ws |> String.concat ", " |> strf "Installable on Windows %s" ]
                match rel.IncludedInServer with
                | Some server -> yield li [] [ strf "Included in Windows Server %s" server ]
                | None -> ()
                match rel.InstallableOnServer with
                | [] -> ()
                | ws -> yield li [] [ ws |> String.concat ", " |> strf "Installable on Windows Server %s" ]
                match rel.NewFeaturesLink with
                | Some link -> yield li [] [ a [ _href link ] [ str "New features" ] ]
                | None -> ()
                match rel.NewAccessibilityLink with
                | Some link -> yield li [] [ a [ _href link ] [ str "New in accessibility" ] ]
                | None -> ()
            ]
            section [ _class "release-notes" ] [
                match rel.ReleaseNotes with
                | Some { Link = url; Markdown = Some md } ->
                    yield div [ _class "header-box inner-spaced" ] [
                        h2 [] [ str "Release notes" ]
                        span [] [ str "("; a [ _href url ] [ str "Source" ]; str ")" ]
                    ]
                    yield article [ _class "text" ] [ rawText (Markdown.ToHtml(md, mdPipeline url)) ]
                | Some { Link = url } ->
                    yield h2 [ _class "inner-spaced" ] [ str "Release notes" ]
                    yield p [ _class "text" ] [ 
                        str "These release notes could not be displayed. Find them here: "
                        a [ _href url ] [ str "Release notes" ] 
                    ]
                | None ->
                    yield h2 [ _class "inner-spaced" ] [ str "Release notes" ]
                    yield p [ _class "text" ] [
                        strf "No release notes available for %O" rel.Version
                    ]
            ]
        ]
       