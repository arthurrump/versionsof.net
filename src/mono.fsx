module Mono

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

open Markdig
open NetCore.Versions
open NetCore.Versions.Data
open Nett
open Thoth.Json.Net

// Data
///////
type ReleaseDate =
    | Released of DateTime
    | Skipped
    | Stub

    static member Encoder = function
        | Released dt -> Encode.datetime dt
        | Skipped -> Encode.string "skipped"
        | Stub -> Encode.string "stub"

    static member Decoder path value = 
        match Decode.string path value with
        | Ok "skipped" -> Ok Skipped
        | Ok "stub" -> Ok Stub
        | Ok (DateTime dt) -> Ok (Released dt)
        | Ok _ -> Error (path, BadPrimitive ("a date, 'skipped' or 'stub'", value))
        | Error v -> Error v

let (|ReleaseDate|_|) = function
    | "skipped" -> Some Skipped
    | t when String.isNullOrWhiteSpace t -> Some Stub
    | DateTime dt -> Some (Released dt)
    | t -> Trace.traceImportantfn "Unparsed release date: \"%s\"" t; None

type Release =
    { Version : Version
      ReleaseDate : ReleaseDate
      Markdown : string
      ReleaseNotesSource : Url }

let ghFileDecoder =
    Decode.object (fun get ->
        get.Required.Field "name" Decode.string, 
        get.Required.Field "download_url" Decode.string)

let releasesGithubUrl config =
    sprintf "https://api.github.com/repos/%s/contents/%s"
        config.MonoRepo config.MonoPath

let releasesGithubHeaders config =
    let auth = sprintf "%s:%s" config.GitHubClientId config.GitHubClientSecret |> base64
    [ ("Authorization", sprintf "Basic %s" auth) ]

let tryGetReleaseUrls url headers = 
    async {
        let! json = getJsonH url headers
        return json 
        |> Result.bind (Decode.fromString (Decode.list ghFileDecoder))
        |> Result.map (List.filter (fun (file, _) -> file <> "index.md") >> List.map snd)
    }

let yamlDecode = 
    let deserializer = YamlDotNet.Serialization.Deserializer()
    fun (text : string) ->
        deserializer.Deserialize<Collections.Generic.Dictionary<string, string>>(text)

let tryParseRelease sourceUrl text =
    match Markdown.splitFrontmatter text with
    | (Some yaml, md) ->
        let fields = yamlDecode yaml
        match fields.["version"], fields.["releasedate"] with
        | Version.Version v, ReleaseDate d ->
            Ok { Version = v; ReleaseDate = d; Markdown = md; ReleaseNotesSource = sourceUrl }
        | _ -> 
            Error (sprintf "Couldn't parse Mono release %s" sourceUrl)
    | _ -> 
        Error (sprintf "No frontmatter found in Mono release %s" sourceUrl)

let tryGetReleasesFromUrls urls =
    async {
        let! files =
            urls
            |> List.map (fun url ->
                downloadGh "text/plain" url
                |> Async.map (Result.bind (tryParseRelease url)))
            |> Async.Parallel
        return files |> List.ofArray |> Result.allOk
    }

let tryGetReleases config =
    async {
        let! urls = tryGetReleaseUrls (releasesGithubUrl config) (releasesGithubHeaders config)
        match urls with
        | Ok urls -> return! tryGetReleasesFromUrls urls
        | Error e -> return Error e
    }

// Pages
////////
type Info =
    { LatestRelease : Release
      ReleasesSubset : Release list }

type Page =
    | ReleasesOverview of Release list * Info
    | ReleasePage of Release

let releaseUrl rel = sprintf "/mono/%O/" rel.Version

let getInfo monoReleases =
    let released =
        monoReleases 
        |> List.filter (fun rel -> match rel.ReleaseDate with Released _ -> true | _ -> false)
        |> List.sortByDescending (fun rel -> rel.Version)
    { LatestRelease = released |> List.head
      ReleasesSubset = released |> List.take 3 }

let releasesToPages releases =
    [ yield { Url = "/mono/"; Content = ReleasesOverview (releases, getInfo releases) }
      for rel in releases -> { Url = releaseUrl rel; Content = ReleasePage rel } ]

let getBreadcrumbs = function
    | ReleasesOverview _ -> []
    | ReleasePage _ -> [ ("Mono", "/mono/") ]

// Template
///////////
let titleText = function
    | ReleasesOverview _ -> "Mono - "
    | ReleasePage rel -> sprintf "Mono %O - " rel.Version

let keywords page =
    [ "Mono"; "Mono Version" ] @
    match page with
    | ReleasesOverview _ -> []
    | ReleasePage rel -> 
        [ for s in [ "Mono"; "Mono Version"; "Mono Release" ] -> 
            sprintf "%s %O" s rel.Version ]

let description = function
    | ReleasesOverview _ ->
        "Releases of Mono. "
    | ReleasePage rel ->
        match rel.ReleaseDate with
        | Released relDate ->
            sprintf "Release %O of Mono, released on %a. " rel.Version date relDate
        | Skipped ->
            sprintf "Skipped release %O of Mono. " rel.Version
        | Stub ->
            sprintf "Release %O of Mono. " rel.Version

let private releasesTable releases =
    div [ _class "table-wrapper" ] [
        table [ _class "overview-table mono-table" ] [
            thead [] [ tr [] [
                th [] [ str "Version" ]
                th [] [ str "Release date" ]
            ] ]
            tbody [] [
                for rel in releases -> tr [ _onclick (sprintf "location.pathname = '%s';" (releaseUrl rel)) ] [
                    td [ _class "title" ] [ a [ _href (releaseUrl rel) ] [ strf "%O" rel.Version ] ]
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

let private header monoInfo =
    div [ _class "inner-spaced latest-versions" ] [
        div [] [
            span [ _class "version" ] [
                a [ _href (releaseUrl monoInfo.LatestRelease) ] 
                  [ strf "%O" monoInfo.LatestRelease.Version ]
            ]
            span [ _class "label" ] [ str "Latest release" ]
        ]
    ]

let homeSection info =
    section [] [
        h1 [ _class "inner-spaced" ] [ a [ _href "/mono/" ] [ str "Mono" ] ]
        header info
        h2 [ _class "inner-spaced" ] [ str "Recent releases" ]
        releasesTable info.ReleasesSubset
        a [_class "inner-spaced"; _href "/mono/" ] [ str "See all releases >" ]
    ]

let content = function
    | ReleasesOverview (releases, monoInfo) ->
        let releases = releases |> List.sortByDescending (fun rel -> rel.Version)
        div [ _class "inner-container" ] [
            h1 [ _class "inner-spaced" ] [ str "Mono" ]
            header monoInfo
            h2 [ _class "inner-spaced" ] [ str "Releases" ]
            releasesTable releases
        ]
    | ReleasePage rel ->
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
       