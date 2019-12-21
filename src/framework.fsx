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
open FsToolkit.ErrorHandling.AsyncResultCEExtensions
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
      IncludedInVisualStudio : string option
      IncludedInWindows : string list
      IncludedInServer : string list
      InstallableOnWindows : string list
      InstallableOnServer : string list
      ReleaseNotes : ReleaseNotes option
      NewFeaturesLink : Url option
      NewAccessibilityLink : Url option }

let [<Literal>] msDocsUrl = "https://docs.microsoft.com/en-us/dotnet/framework/migration-guide/versions-and-dependencies"
let [<Literal>] lifecycleUrl = """https://support.microsoft.com/api/lifecycle/GetProductsLifecycle?query={"names":[".NET%20Framework"],"years":"0","gdsId":0,"export":false}"""

type MsDocs = HtmlProvider<msDocsUrl, IncludeLayoutTables = true>
type Lifecycle = JsonProvider<lifecycleUrl>

let docsLinkToUrl (docUrl : string) (link : HtmlNode) =
    let rec combineUrl (baseUrl : string) (suffix : string) =
        let baseUrl = baseUrl.TrimEnd('/').Substring(0, baseUrl.LastIndexOf '/')
        if suffix.StartsWith("../") 
        then combineUrl baseUrl (suffix.Substring(3))
        else baseUrl + "/" + suffix

    Option.option {
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

let getVersionDocs (versionLinks : HtmlNode) = 
    versionLinks.DirectInnerText().Split('\n').[0].Trim() |> Version.parse
let getVersionLifecycle (name : string) =
    name.Replace("Microsoft", "")
        .Replace(".NET Framework", "")
        .Trim()
    |> Version.parse

let rowToRelease (docsRow : HtmlNode) (lifecycle : Lifecycle.Root) =
    asyncResult {
        match docsRow.CssSelect("td") with
        | [ versionLinks; clrVersion; vsVersion; windowsVersions; serverVersions; _ ] ->
            let links = versionLinks.CssSelect("a")
            let features = links |> findLinkUrl "New features"
            let accessibility = links |> findLinkUrl "New in accessibility"
            let releaseNotes = links |> findLinkUrl "Release notes"
            let! releaseNotesMarkdown = getReleaseNotesMarkdown releaseNotes

            let! version = versionLinks |> getVersionDocs |> Result.ofOption "Couldn't parse Framework version."
            let! clr = 
                clrVersion.DirectInnerText() 
                |> Version.parse 
                |> Result.ofOption "Couldn't parse Framework CLR version."

            let partitionVersions (versionsTd : HtmlNode) =
                if versionsTd.InnerText().Trim() = "-" then
                    [], []
                else
                    let versions = 
                        versionsTd.InnerText().Split('\n')
                        |> Array.map (fun s -> 
                            s.Replace(",", "")
                             .Replace("Windows Server", "")
                             .Trim())
                        |> Array.filter (not << String.IsNullOrEmpty)
                        |> Array.toList
                    let inst, incl = versions |> List.partition (fun s -> s.StartsWith("+") || s.EndsWith("*"))
                    let inst = inst |> List.map (fun s -> s.Replace("*", "").Substring(if s.StartsWith("+") then 1 else 3).Trim())
                    let incl = incl |> List.map (fun s -> s.Substring(3).Trim())
                    inst, incl
            
            let instWindows, inclWindows = partitionVersions windowsVersions
            let instServer, inclServer = partitionVersions serverVersions
            let vsVersion = 
                let v = vsVersion.InnerText()
                if String.IsNullOrWhiteSpace v 
                then None
                else Some (v.Trim())

            return 
                { Version = version
                  ReleaseDate = lifecycle.StartDate
                  ClrVersion = clr
                  IncludedInVisualStudio = vsVersion
                  IncludedInWindows = inclWindows
                  IncludedInServer = inclServer
                  InstallableOnWindows = instWindows
                  InstallableOnServer = instServer
                  ReleaseNotes = releaseNotes |> Option.map (fun url -> { Link = url; Markdown = releaseNotesMarkdown })
                  NewFeaturesLink = features
                  NewAccessibilityLink = accessibility }
        | _ -> return! Error "Unexpected amount of columns in Framework table."
    }

let tryGetReleases () =
    let nv = Option.map (Version.pad 3)
    async {
        let! msDocs = MsDocs.AsyncGetSample()
        let docsTable = msDocs.Html.CssSelect("main#main table") |> List.head
        let docsRows = docsTable.CssSelect("tbody tr")
        let! lifecycle = Lifecycle.AsyncGetSamples()
        return!
            docsRows
            |> List.map (fun dr ->
                let lr = 
                    lifecycle
                    |> Array.find (fun l -> nv (getVersionLifecycle l.Name) = nv (getVersionDocs (dr.CssSelect("td").[0])))
                rowToRelease dr lr)
            |> Async.Parallel
            |> Async.map (List.ofArray >> Result.allOk)
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
                    td [ _class ("windows" + if List.isEmpty rel.IncludedInWindows then " unknown" else "") ] [
                        match rel.IncludedInWindows with
                        | [] ->
                            yield str "-"
                        | versions ->
                            yield span [ _class "label" ] [ str "Included in Windows " ]
                            yield str (versions |> List.last)
                    ]
                    td [ _class ("server" + if List.isEmpty rel.IncludedInServer then " unknown" else "") ] [
                        match rel.IncludedInServer with
                        | [] ->
                            yield str "-"
                        | versions ->
                            yield span [ _class "label" ] [ str "Included in Windows Server " ]
                            yield str (versions |> List.last)
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
                | [] -> ()
                | ws -> yield li [] [ ws |> String.concat ", " |> strf "Included in Windows %s" ]
                match rel.InstallableOnWindows with
                | [] -> ()
                | ws -> yield li [] [ ws |> String.concat ", " |> strf "Installable on Windows %s" ]
                match rel.IncludedInServer with
                | [] -> ()
                | ws -> yield li [] [ ws |> String.concat ", " |> strf "Included in Windows Server %s" ]
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
       