module Core

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

open Markdig
open NetCore.Versions
open NetCore.Versions.Data
open Thoth.Json.Net

// Data
///////
let private decodeIndex = Decode.fromString (Decode.field "releases-index" (Decode.list IndexEntry.Decoder))
let private decodeChannel = Decode.fromString Channel.Decoder

let private tryGetIndex url =
    async { let! json = getJsonGh url in return json |> Result.bind decodeIndex }

let private tryGetChannel url =
    async { let! json = getJsonGh url in return json |> Result.bind decodeChannel }

let private tryGetChannelsForIndex = 
    List.map (fun i -> tryGetChannel i.ReleasesJson)
    >> Async.Parallel
    >> Async.map (List.ofArray >> Result.allOk)
    // If there is no current, we want to make the latest LTS release "current-lts"
    >> Async.map (Result.map (fun channels ->
        if channels |> List.exists (fun ch -> ch.SupportPhase = "current") then
            channels
        else
            let currentLts = 
                channels 
                |> List.filter (fun ch -> ch.SupportPhase = "lts")
                |> List.maxBy (fun ch -> ch.ChannelVersion)
            channels
            |> List.filter ((<>) currentLts)
            |> List.append [ { currentLts with SupportPhase = "current-lts" } ]
            |> List.sortByDescending (fun ch -> ch.ChannelVersion)
    ))

let tryGetChannels indexUrl =
    async {
        match! tryGetIndex indexUrl with
        | Ok index -> return! tryGetChannelsForIndex index
        | Error e -> return Error e   
    }

let private getMdReleaseNoteLinks channels =
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

// Pages
////////
type Info =
    { LatestRuntime : Version
      LatestRuntimeUrl : Url
      LatestSdk : Version
      LatestSdkUrl : Url
      PrimaryChannels : Channel list }

type Page =
    | ChannelsOverview of Channel list * Info
    | ChannelPage of Channel
    | ReleasePage of {| Channel : Channel; Release : Release; ReleaseNotesMarkdown : string option |}

let channelUrl ch = sprintf "/core/%O/" ch.ChannelVersion
let releaseUrl ch rel = sprintf "/core/%O/%O/" ch.ChannelVersion rel.ReleaseVersion

let getInfo channels =
    let current = 
        channels
        |> List.filter (fun ch -> ch.SupportPhase <> "preview" && ch.SupportPhase <> "rc")
        |> List.maxBy (fun ch -> ch.ChannelVersion)
    { LatestRuntime = current.LatestRuntime
      LatestRuntimeUrl = releaseUrl current (getLatestRuntimeRel current)
      LatestSdk = current.LatestSdk
      LatestSdkUrl = releaseUrl current (getLatestSdkRel current)
      PrimaryChannels = channels |> List.filter (fun ch -> [ "current"; "current-lts"; "lts"; "rc" ] |> List.contains ch.SupportPhase) }

let channelsToPages channels releaseNotesMap =
    [ yield { Url = "/core/"; Content = ChannelsOverview (channels, getInfo channels) }
      for ch in channels do
        yield { Url = channelUrl ch; Content = ChannelPage ch }
        for rel in ch.Releases do
            yield { Url = releaseUrl ch rel
                    Content = Page.ReleasePage
                        {| Channel = ch
                           Release = rel
                           ReleaseNotesMarkdown = releaseNotesMap |> Map.tryFind rel.ReleaseNotes |} } ]

let getBreadcrumbs page =
    let ov = (".NET Core", "/core/")
    match page with
    | ChannelsOverview _ -> []
    | ChannelPage _ -> [ ov ]
    | ReleasePage rel -> [ ov; (sprintf "Channel %O" rel.Channel.ChannelVersion, channelUrl rel.Channel) ]

// Template
///////////
let titleText = function
    | ChannelsOverview _ -> ".NET Core - "
    | ChannelPage ch -> sprintf "Channel %O - .NET Core - " ch.ChannelVersion
    | ReleasePage rel -> sprintf ".NET Core %O - " rel.Release.ReleaseVersion

let keywords page =
    [ ".NET Core" ] @
    match page with
    | ChannelsOverview _ -> []
    | ChannelPage ch -> 
        [ for s in [ ".NET"; ".NET Core"; ".NET channel"; ".NET Core channel"; "dotnet"; "dotnet channel" ] -> 
            sprintf "%s %O" s ch.ChannelVersion ]
    | ReleasePage rel -> 
        [ for s in [ ".NET"; ".NET Core"; ".NET Version"; ".NET Core Version"; "dotnet"; "dotnet version"
                     ".NET Core Release"; "dotnet release" ] -> 
            sprintf "%s %O" s rel.Release.ReleaseVersion ]

let description = function
    | ChannelsOverview _ -> 
        "Channels of .NET Core. "
    | ChannelPage ch -> 
        sprintf "Channel %O of .NET Core, with latest release %O, latest runtime %O, latest SDK %O. " 
            ch.ChannelVersion ch.LatestRelease ch.LatestRuntime ch.LatestSdk
    | ReleasePage rel ->
        sprintf "Release %O of .NET Core, released on %a. " 
            rel.Release.ReleaseVersion date rel.Release.ReleaseDate

let private supportIndicator supportPhase =
    let indicator = indicatorSymb []
    match supportPhase with
    | "preview" ->     indicator "Preview" "border-black"
    | "rc" ->          indicator "Release Candidate" "border-black"
    | "current" ->     indicator "Current" "green"
    | "lts" ->         indicator "Long Term Support" "yellow"
    | "current-lts" -> indicator "Current (LTS)" "green"
    | "eol" ->         indicator "End of Life" "red"
    | "maintenance" -> indicator "Maintenance" "orange"
    | t -> indicatorSymb [ str "?" ] t "border-black"

let private channelsTable channels =
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

let private header info =
    div [ _class "inner-spaced latest-versions" ] [
        div [] [ 
            span [ _class "version" ] [ 
                a [ _href info.LatestRuntimeUrl ] [ strf "%O" info.LatestRuntime ]
            ]
            span [ _class "label" ] [ str "Latest runtime" ]
        ]
        div [] [ 
            span [ _class "version" ] [
                a [ _href info.LatestSdkUrl ] [ strf "%O" info.LatestSdk ]
            ]
            span [ _class "label" ] [ str "Latest SDK" ]
        ]
    ]

let homeSection info =
    section [] [
        h1 [ _class "inner-spaced" ] [ a [ _href "/core/" ] [ str ".NET Core" ] ]
        header info
        h2 [ _class "inner-spaced" ] [ str "Supported channels" ]
        channelsTable info.PrimaryChannels
        a [ _class "inner-spaced"; _href "/core/" ] [ str "See all channels >" ]
    ]

let content = function
    | ChannelsOverview (channels, info) -> 
        div [ _class "inner-container" ] [
            h1 [ _class "inner-spaced" ] [ str ".NET Core" ]
            header info
            h2 [ _class "inner-spaced" ] [ str "Channels" ]
            channelsTable channels
        ]
    | ChannelPage channel ->
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
    | ReleasePage releaseAndNotes ->
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
                match rel.WindowsDesktop with
                | Some win -> yield li [] [ strf "Windows Desktop Runtime %O" win.Version ]
                | None -> ()
                if rel.Security then yield li [] [ indicatorSymb [ str "!" ] "Security" "border-red" ]
            ]
            for sdk in allSdks rel do
                let props = 
                    [ match sdk.VsVersion with 
                      | [] -> match sdk.VsSupport with Some v -> yield li [] [ str v ] | _ -> ()
                      | vs -> for v in vs -> li [] [ strf "Visual Studio %O" v ] 
                      match sdk.VsMacVersion with
                      | Some v -> yield li [] [ strf "Visual Studio for Mac %O" v ]
                      | _ -> match sdk.VsMacSupport with Some v when String.isNotNullOrEmpty v -> yield li [] [ str v ] | _ -> ()
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
                    | Some symb -> 
                        if (not << List.isEmpty) symb.Files then 
                            yield filesList "Symbols" symb.Files
                    | None -> ()
                    match rel.WindowsDesktop with
                    | Some win -> yield filesList "Windows Desktop Runtime" win.Files
                    | None -> ()
                ]
            ]
        ]
    