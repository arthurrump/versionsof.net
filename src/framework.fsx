module Framework
open System.Text.RegularExpressions

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
open Fake.StaticGen.Markdown

open System

open FSharp.Data
open FSharpPlus.Builders
open Markdig
open NetCore.Versions
open NetCore.Versions.Data
open Nett
open Thoth.Json.Net

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
type MsDocs = HtmlProvider<msDocsUrl>

let wikiString =
    let regex = new Regex @"\[[^\]]*\]\s*"
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

let rowToRelease (wikiRow : Wikipedia.OverviewOfNetFrameworkReleaseHistory123.Row) =
    monad {
        let! version = wikiRow.``Version number`` |> wikiVersion
        let! date = wikiRow.``Release date`` |> wikiDate
        let! clr = wikiRow.``CLR version`` |> string |> wikiVersion
        let windows = wikiRow.``Included in - Windows`` |> wikiString
        let server = wikiRow.``Included in - Windows Server`` |> wikiString
        let installWindows = wikiRow.``Can be installed on[4] - Windows`` |> wikiList
        let installServer = wikiRow.``Can be installed on[4] - Windows Server`` |> wikiList
        return 
            { Version = version
              ReleaseDate = date
              ClrVersion = clr
              IncludedInWindows = windows
              IncludedInServer = server
              InstallableOnWindows = installWindows
              InstallableOnServer = installServer
              ReleaseNotes = None
              NewFeaturesLink = None
              NewAccessibilityLink = None }
    }

let tryGetReleases config =
    async {
        let! wiki = Wikipedia.AsyncGetSample()
        return
            wiki.Tables.``Overview of .NET Framework release history[1][2][3]``.Rows
            |> List.ofArray
            |> List.map (fun row -> 
                rowToRelease row 
                |> Result.mapError (sprintf "Error parsing Framework %A: %s" row.``Version number``))
            |> Result.allOk
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
        ]
       