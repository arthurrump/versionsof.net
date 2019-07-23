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

type Wikipedia = HtmlProvider<wikipediaUrl, PreferOptionals = true>
type MsDocs = HtmlProvider<msDocsUrl>

let wikiString (str : string) =
    if str.Contains "[" then str.Substring(0, str.IndexOf "[") else str

let wikiParse tryParse err str =
    let str = wikiString str
    match tryParse str with
    | Some res -> Ok res
    | None -> Error (err str)

let wikiVersion = wikiParse Version.parse (sprintf "Couldn't parse '%s' as a version")
let wikiDate = wikiParse DateTime.tryParse (sprintf "Couldn't parse '%s' as a date")

let rowToRelease (wikiRow : Wikipedia.OverviewOfNetFrameworkReleaseHistory123.Row) =
    let (|*>) x f = Option.bindResult "Value is None" f x
    monad {
        let! version = wikiRow.``Version number`` |*> wikiVersion
        let! date = wikiRow.``Release date`` |*> wikiDate
        let! clr = wikiRow.``CLR version`` |> string |> Some |*> wikiVersion
        let windows = wikiRow.``Included in - Windows``
        let server = wikiRow.``Included in - Windows Server``
        let installWindows = wikiRow.``Can be installed on[4] - Windows`` |> Option.map (String.split ',') |> Option.defaultValue []
        let installServer = wikiRow.``Can be installed on[4] - Windows Server`` |> Option.map (String.split ',') |> Option.defaultValue []
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
            ] ]
            tbody [] [
                for rel in releases -> tr [ _onclick (sprintf "location.pathname = '%s';" (releaseUrl rel)) ] [
                    td [ _class "title" ] [ a [ _href (releaseUrl rel) ] [ strf "%O" rel.Version ] ]
                    td [ _class "rel-date" ] [
                        yield span [ _class "label" ] [ str "Released on " ]
                        yield strf "%a" date rel.ReleaseDate
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
                li [] [ strf "Released on %a" date rel.ReleaseDate ]
            ]
        ]
       