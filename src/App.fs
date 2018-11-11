namespace VersionsOfDotNet

open Elmish
open Elmish.React
open Fable.Helpers.React
open Fable.Helpers.React.Props
open Data
open Fable.PowerPack
open System
open Search

type Loadable<'t> =
    | Unloaded
    | Loading
    | Error of exn
    | Loaded of 't

[<RequireQualifiedAccess>]
module Loadable =
    let isLoaded x =
        match x with
        | Loaded _ -> true
        | _ -> false

    let map f x =
        match x with
        | Unloaded -> Unloaded | Loading -> Loading | Error e -> Error e
        | Loaded t -> Loaded (f t)

type ReleaseModel =
    { Release: Release
      Expanded: bool }

type ChannelInfo =
    { LifecyclePolicy: Url
      Releases: ReleaseModel list }

type ChannelModel =
    { Index: IndexEntry
      Info: Loadable<ChannelInfo>
      Expanded: bool }

type LoadedModel =
    { Channels: ChannelModel list
      Search: Search.Model }

type Model = Loadable<LoadedModel>

type File =
    | Index
    | Channel of Url

type Msg =
    | LoadIndex
    | LoadChannel of Url
    | FetchedIndex of IndexEntry list
    | FetchedChannel of Url * Channel
    | FetchError of File * exn
    | ExpandChannel of Url
    | CollapseChannel of Url
    | ExpandRelease of Url * ReleaseModel
    | CollapseRelease of Url * ReleaseModel
    | Search of Search.Msg

module App =
    let fetchError file ex = FetchError (file, ex)

    let fetchIndexCmd = 
        Cmd.ofPromise Fetch.index () FetchedIndex (fetchError Index)

    let fetchChannelCmd githubUrl =
        Cmd.ofPromise Fetch.channel githubUrl FetchedChannel (fetchError (Channel githubUrl))

    let setChannelInfoFor releasesUrl info =
        List.map (fun channel -> if channel.Index.ReleasesJson = releasesUrl
                                 then { channel with Info = info }
                                 else channel)

    let setExpandedFor releasesUrl expanded =
        List.map (fun channel -> if channel.Index.ReleasesJson = releasesUrl
                                 then { channel with Expanded = expanded }
                                 else channel) 

    let setExpandedForReleaseInInfo releaseModel expanded (info: ChannelInfo) =
        { info with 
            Releases =
                info.Releases
                |> List.map (fun rm -> if rm = releaseModel
                                       then { rm with Expanded = expanded }
                                       else rm) }

    let setExpandedForRelease releasesUrl releaseModel expanded =
        List.map (fun channel -> if channel.Index.ReleasesJson = releasesUrl
                                 then { channel with Info = channel.Info 
                                                            |> Loadable.map (setExpandedForReleaseInInfo releaseModel expanded) }
                                 else channel)

    let releaseModelToRelease rm =
        rm.Release

    let releaseToReleaseModel expanded r =
        { Release = r; Expanded = expanded }

    let init () = Loading, fetchIndexCmd

    let latestNonPreviewIndexEntry : (IndexEntry list -> IndexEntry) = 
        List.filter (fun i -> i.SupportPhase <> "preview")
        >> List.maxBy (fun i -> i.ChannelVersion)

    let latestNonPreviewChannel = 
        List.filter (fun i -> i.Index.SupportPhase <> "preview")
        >> List.maxBy (fun i -> i.Index.ChannelVersion)

    let updateChannels f model =
        { model with Channels = model.Channels |> f }

    let update (msg:Msg) (model:Model) =
        match msg with
        | LoadIndex ->
            Loading, fetchIndexCmd
        | LoadChannel url -> 
            model |> Loadable.map (updateChannels (setChannelInfoFor url Loading)), fetchChannelCmd url
        | FetchedIndex indices -> 
            let latestChannelUrl = 
                indices 
                |> latestNonPreviewIndexEntry
                |> fun i -> i.ReleasesJson
            let channels = indices |> List.map (fun i -> { Index = i; Info = Unloaded; Expanded = false })
            Loaded { Channels = channels
                     Search = Search.init () }, 
            Cmd.ofMsg (LoadChannel latestChannelUrl)
        | FetchedChannel (url, channel) -> 
            let info = { LifecyclePolicy = channel.LifecyclePolicy
                         Releases = channel.Releases |> List.map (releaseToReleaseModel false) }
            model |> Loadable.map (updateChannels (setChannelInfoFor url (Loaded info))), Cmd.none
        | FetchError (file, ex) -> 
            match file with
            | Index -> Error ex, Cmd.none
            | Channel url ->
                model |> Loadable.map (updateChannels (setChannelInfoFor url (Error ex))), Cmd.none
        | ExpandChannel url ->
            let loaded = 
                match model with
                | Loaded cs ->
                    match cs.Channels |> List.tryFind (fun c -> c.Index.ReleasesJson = url) with
                    | Some c -> Loadable.isLoaded c.Info
                    | None -> false
                | _ -> false
            model |> Loadable.map (updateChannels (setExpandedFor url true)), 
            if loaded then Cmd.none else Cmd.ofMsg (LoadChannel url)
        | CollapseChannel url ->
            model |> Loadable.map (updateChannels (setExpandedFor url false)), Cmd.none
        | ExpandRelease (channelUrl, releaseModel) ->
            model |> Loadable.map (updateChannels (setExpandedForRelease channelUrl releaseModel true)), Cmd.none
        | CollapseRelease (channelUrl, releaseModel) ->
            model |> Loadable.map (updateChannels (setExpandedForRelease channelUrl releaseModel false)), Cmd.none
        | Search message -> 
            model |> Loadable.map (fun m -> { m with Search = Search.update message m.Search }), Cmd.none

    let dateToHtmlTime (date: DateTime) = 
        let s = date.ToString("yyyy-MM-dd")
        time [ Props.DateTime s ] [ str s ]

    let chevronRight props = button props [ img [ Src "data:image/svg+xml;base64,PD94bWwgdmVyc2lvbj0iMS4wIiA/PjwhRE9DVFlQRSBzdmcgIFBVQkxJQyAnLS8vVzNDLy9EVEQgU1ZHIDEuMS8vRU4nICAnaHR0cDovL3d3dy53My5vcmcvR3JhcGhpY3MvU1ZHLzEuMS9EVEQvc3ZnMTEuZHRkJz48c3ZnIGhlaWdodD0iNTEycHgiIGlkPSJMYXllcl8xIiBzdHlsZT0iZW5hYmxlLWJhY2tncm91bmQ6bmV3IDAgMCA1MTIgNTEyOyIgdmVyc2lvbj0iMS4xIiB2aWV3Qm94PSIwIDAgNTEyIDUxMiIgd2lkdGg9IjUxMnB4IiB4bWw6c3BhY2U9InByZXNlcnZlIiB4bWxucz0iaHR0cDovL3d3dy53My5vcmcvMjAwMC9zdmciIHhtbG5zOnhsaW5rPSJodHRwOi8vd3d3LnczLm9yZy8xOTk5L3hsaW5rIj48cGF0aCBkPSJNMjk4LjMsMjU2TDI5OC4zLDI1NkwyOTguMywyNTZMMTMxLjEsODEuOWMtNC4yLTQuMy00LjEtMTEuNCwwLjItMTUuOGwyOS45LTMwLjZjNC4zLTQuNCwxMS4zLTQuNSwxNS41LTAuMmwyMDQuMiwyMTIuNyAgYzIuMiwyLjIsMy4yLDUuMiwzLDguMWMwLjEsMy0wLjksNS45LTMsOC4xTDE3Ni43LDQ3Ni44Yy00LjIsNC4zLTExLjIsNC4yLTE1LjUtMC4yTDEzMS4zLDQ0NmMtNC4zLTQuNC00LjQtMTEuNS0wLjItMTUuOCAgTDI5OC4zLDI1NnoiLz48L3N2Zz4=" ] ]
    let chevronDown  props = button props [ img [ Src "data:image/svg+xml;base64,PD94bWwgdmVyc2lvbj0iMS4wIiA/PjwhRE9DVFlQRSBzdmcgIFBVQkxJQyAnLS8vVzNDLy9EVEQgU1ZHIDEuMS8vRU4nICAnaHR0cDovL3d3dy53My5vcmcvR3JhcGhpY3MvU1ZHLzEuMS9EVEQvc3ZnMTEuZHRkJz48c3ZnIGhlaWdodD0iNTEycHgiIGlkPSJMYXllcl8xIiBzdHlsZT0iZW5hYmxlLWJhY2tncm91bmQ6bmV3IDAgMCA1MTIgNTEyOyIgdmVyc2lvbj0iMS4xIiB2aWV3Qm94PSIwIDAgNTEyIDUxMiIgd2lkdGg9IjUxMnB4IiB4bWw6c3BhY2U9InByZXNlcnZlIiB4bWxucz0iaHR0cDovL3d3dy53My5vcmcvMjAwMC9zdmciIHhtbG5zOnhsaW5rPSJodHRwOi8vd3d3LnczLm9yZy8xOTk5L3hsaW5rIj48cGF0aCBkPSJNMjU2LDI5OC4zTDI1NiwyOTguM0wyNTYsMjk4LjNsMTc0LjItMTY3LjJjNC4zLTQuMiwxMS40LTQuMSwxNS44LDAuMmwzMC42LDI5LjljNC40LDQuMyw0LjUsMTEuMywwLjIsMTUuNUwyNjQuMSwzODAuOSAgYy0yLjIsMi4yLTUuMiwzLjItOC4xLDNjLTMsMC4xLTUuOS0wLjktOC4xLTNMMzUuMiwxNzYuN2MtNC4zLTQuMi00LjItMTEuMiwwLjItMTUuNUw2NiwxMzEuM2M0LjQtNC4zLDExLjUtNC40LDE1LjgtMC4yTDI1NiwyOTguMyAgeiIvPjwvc3ZnPg==" ] ]

    let supportIndicator supportPhase =
        match supportPhase with
        | "preview" ->
            [ span [ Class "status-indicator preview"
                     Title "Preview" ] [ ]
              span [ ] [ str "Preview" ] ]
        | "lts" -> 
            [ span [ Class "status-indicator lts"
                     Title "Long Term Support" ] [ ]
              span [ ] [ str "Long Term Support" ] ]
        | "eol" -> 
            [ span [ Class "status-indicator eol"
                     Title "End of Life" ] [ ]
              span [ ] [ str "End of Life" ] ]
        | "maintenance" -> 
            [ span [ Class "status-indicator maintenance"
                     Title "Maintenance" ] [ ]
              span [ ] [ str "Maintenance" ] ]
        | t -> 
            [ span [ Class "status-indicator unknown" 
                     Title t ] [ str "?" ]
              span [ ] [ str t ] ]

    let securityIndicator release = 
        if release.Security then
            [ div [ Class "status-box" ]
                  [ span [ Class "status-indicator security"
                           Title "Security" ] [ str "!" ]
                    span [ ] [ str "Security" ] ] ]
        else [ ]

    let errorView (ex: exn) retryFun =
        [ span [ Class "error-symbol" ] 
               [ str "×" ]
          span [ Class "error-title" ] 
               [ str "An error occurred." ]
          button [ Class "error-retry-button"
                   OnClick retryFun ] 
                 [ str "Try again" ]
          span [ Class "error-details" ]
               [ str (sprintf "Details: %s" ex.Message) ] ]

    let headRow =
        tr [ ]
           [ th [ Class "hide-border" ] [ ]
             th [ ] [ ]
             th [ ] [ str "Channel" ]
             th [ ] [ str "Latest release" ]
             th [ ] [ str "Latest release date" ]
             th [ ] [ str "Support" ]
             th [ ] [ str "End of Life date" ] ]

    let str (v: obj) = string v |> str

    let channelRow dispatch c =
        let i = c.Index
        let toggleExpand _ =
            dispatch (if c.Expanded
                      then CollapseChannel i.ReleasesJson
                      else ExpandChannel i.ReleasesJson)
        tr [ OnClick toggleExpand ]
           [ td [ Class "expand-button"
                  ColSpan 2 ] 
                [ (if c.Expanded then chevronDown else chevronRight) [ ] ]
             td [ ] [ str i.ChannelVersion ]
             td [ ] [ str i.LatestRelease ]
             td [ ] [ dateToHtmlTime i.LatestReleaseDate ]
             td [ ] [ div [ Class "status-box" ] ( supportIndicator i.SupportPhase ) ]
             td [ ] [ ( match i.EolDate with
                        | Some d -> dateToHtmlTime d
                        | None -> str "-" ) ] ]

    let expandedRelease rm =
        let r = rm.Release

        let fullRuntimeVersion (runtime: Runtime option) =
            runtime
            |> Option.exists (fun r -> r.VersionDisplay 
                                       |> Option.exists (fun vd -> r.Version |> Version.displayedAs vd))

        let fullSdkVersion (sdk: Sdk) =
            sdk.VersionDisplay |> Option.exists (fun vd -> sdk.Version |> Version.displayedAs vd)

        let lif fmt = Printf.kprintf (fun s -> li [ ] [ str s ]) fmt

        let lia href text = li [ ] [ a [ Href href ] [ str text ] ]

        let (|SomeText|_|) input = 
            match input with
            | Some i when not (String.IsNullOrWhiteSpace(i)) -> Some i
            | _ -> None

        tr [ ]
           [ td [ Class "hide-border" ] [ ]
             td [ Class "hide-border" ] [ ]
             td [ Class "hide-border"
                  ColSpan 5 ]
                [ ul [ Class "expanded-release" ]
                      [ if fullRuntimeVersion r.Runtime then 
                            yield lif "Runtime version %O" r.Runtime.Value.Version
                        if fullSdkVersion r.Sdk then
                            yield lif "Sdk version %O" r.Sdk.Version
                        match r.Sdk.VsVersion with SomeText v -> yield lif "Included in Visual Studio %s" v | _ -> ()
                        match r.Sdk.CsharpLanguage with SomeText v -> yield lif "Supports C# %s" v | _ -> ()
                        match r.Sdk.FsharpLanguage with SomeText v -> yield lif "Supports F# %s" v | _ -> ()
                        match r.Sdk.VbLanguage with SomeText v -> yield lif "Supports Visual Basic %s" v | _ -> ()
                        match r.AspnetcoreRuntime with 
                        | Some a -> 
                            yield lif "ASP.NET Core Runtime %O" a.Version
                            match a.VersionAspnetcoremodule with 
                            | Some a when not a.IsEmpty -> 
                                yield lif "ASP.NET Core IIS Module %O" a.Head
                            | _ -> ()
                        | None -> ()
                        match r.ReleaseNotes with Some url -> yield lia url "Release notes" | None -> () ] ] ]

    let expandedChannel dispatch c last =
        match c.Info with
        | Unloaded | Loading -> 
            [ tr [ ] 
                 [ td [ ColSpan 7 ] 
                      [ div [ Class "expanded-loading" ]
                            [ div [ Class "loading" ] [ ] ] ] ] ]
        | Error ex -> 
            [ tr [ ] 
                 [ td [ ColSpan 7 ] 
                      [ div [ Class "channel-error column" ] 
                            ( errorView ex (fun _ -> dispatch (LoadChannel c.Index.ReleasesJson)) ) ] ] ]
        | Loaded info -> 
            [ tr [ ]
                 [ th [ ] [ ]
                   th [ ] [ ]
                   th [ ] [ str "Version" ]
                   th [ ] [ str "Release date" ]
                   th [ ] [ str "Runtime" ]
                   th [ ] [ str "Sdk" ]
                   th [ ] [ ] ] ] @
            [ for rm in info.Releases do
                  let r = rm.Release
                  yield
                      tr [ OnClick (fun _ -> (c.Index.ReleasesJson, rm)
                                             |> (if rm.Expanded then CollapseRelease else ExpandRelease)
                                             |> dispatch) ]
                         [ td [ Class "hide-border" ] [ ]
                           td [ Class "expand-button" ] 
                              [ (if rm.Expanded then chevronDown else chevronRight) [ ] ]
                           td [ ] [ str (string r.ReleaseVersion) ]
                           td [ ] [ dateToHtmlTime r.ReleaseDate ]
                           td [ ] [ str (match r.Runtime with
                                         | Some r -> Option.defaultValue (string r.Version) r.VersionDisplay
                                         | None -> "-") ]
                           td [ ] [ str (Option.defaultValue (string r.Sdk.Version) r.Sdk.VersionDisplay) ]
                           td [ ] ( securityIndicator r ) ]
                  if rm.Expanded then yield expandedRelease rm ] @
            if last then [ ] else [ headRow ]

    let showChannels dispatch channels =
        if channels |> List.isEmpty then
            [ ]
        else
            [ for c in channels ->
                  [ yield channelRow dispatch c
                    if c.Expanded then yield! expandedChannel dispatch c (c = List.last channels) ] ] 
            |> List.concat

    let releaseFilter filter release =
        let release = release.Release
        match filter with
        | ShowAll -> true
        | WithPrefix (pf, version) ->
            match pf with
            | Release -> release.ReleaseVersion |> Version.matches version
            | Runtime -> release.Runtime |> Option.exists (fun r -> r.Version |> Version.matches version)
            | Sdk -> release.Sdk.Version |> Version.matches version
            | AspRuntime -> release.AspnetcoreRuntime 
                            |> Option.exists (fun a -> a.Version |> Version.matches version)
            | AspModule -> 
                match release.AspnetcoreRuntime with
                | Some { VersionAspnetcoremodule = Some vs } ->
                    vs |> List.exists (Version.matches version)
                | _ -> false
        | Generic version -> 
            release.ReleaseVersion |> Version.matches version ||
            release.Sdk.Version |> Version.matches version ||
            release.Runtime |> Option.exists (fun r -> r.Version |> Version.matches version)

    let filterChannels filter dispatch channels =
        match filter with
        | ShowAll -> showChannels dispatch channels
        | filter -> 
            let filterReleases channelModel =
                let newInfo = 
                    channelModel.Info 
                    |> Loadable.map (fun info -> { info with Releases = List.filter (releaseFilter filter) info.Releases })
                { channelModel with Info = newInfo }

            let filterChannelModel channelModel = 
                match channelModel.Info, filter with
                | _, ShowAll -> true
                | Loaded info, _ -> not info.Releases.IsEmpty
                | _, Generic v -> channelModel.Index.ChannelVersion |> Version.mightMatchInChannel v
                | _, WithPrefix (pf, v) ->
                    match pf with
                    | Release | Runtime | Sdk | AspRuntime -> 
                        channelModel.Index.ChannelVersion |> Version.mightMatchInChannel v
                    | AspModule -> true

            let filtered =
                channels
                |> List.map filterReleases
                |> List.filter filterChannelModel

            do filtered 
               |> List.filter (fun c -> c.Info = Unloaded) 
               |> List.map (fun c -> LoadChannel c.Index.ReleasesJson)
               |> List.iter dispatch

            showChannels dispatch filtered

    let releasesTable dispatch m channels =
        let rows = filterChannels m.Search.Filter dispatch channels

        if rows |> List.isEmpty then
            div [ Class "channel-error column" ] 
                [ span [ Class "error-symbol" ] 
                       [ str "×" ]
                  span [ Class "error-title" ] 
                       [ str "No releases found." ] ]
        else 
            table [ ]       
                  [ thead [ ]
                          [ headRow ]
                    tbody [ ]
                          rows ]

    let view (model:Model) dispatch =
        match model with
        | Unloaded | Loading -> 
            div [ Class "main-loading" ] 
                [ div [ Class "loading" ] [ ] ]
        | Error ex ->
            div [ Class "main-error" ]
                [ div [ Class "container column" ]
                      ( errorView ex (fun _ -> dispatch LoadIndex) ) ]
        | Loaded m ->
            let channels = m.Channels
            let latestReleaseChannel = channels |> latestNonPreviewChannel
            let latestRelease = latestReleaseChannel.Index.LatestRelease
            let latestSdk = 
                latestReleaseChannel.Info 
                |> Loadable.map (fun i -> i.Releases 
                                          |> List.maxBy (fun r -> r.Release.ReleaseDate)
                                          |> fun r -> match r.Release.Sdk.VersionDisplay with
                                                      | Some v -> v
                                                      | None -> string r.Release.Sdk.Version)
            div [ ]
                [ nav [ ]
                      [ div [ Class "container" ]
                            [ h1 [ Id "title" ] [ str "Versions of" ]
                              a [ Href "#core"; Class "active" ] [ str ".NET Core" ]
                              a [ (*Href "#standard"*) Disabled true; Title "Coming soon™"; Style [ Cursor "default"; CSSProp.Color "rgba(255, 255, 255, 0.6)" ] ] [ str ".NET Standard" ]
                              a [ (*Href "#framework"*) Disabled true; Title "Coming soon™"; Style [ Cursor "default"; CSSProp.Color "rgba(255, 255, 255, 0.6)" ] ] [ str ".NET Framework" ]
                            ] ]
                  header [ ]
                         [ div [ Class "container row" ]
                               [ div [ Class "cell" ]
                                     [ span [ Class "version" ] 
                                            [ str latestRelease ]
                                       span [ Class "label" ] 
                                            [ str "Latest runtime" ] ]
                                 div [ Class "cell" ]
                                     [ ( match latestSdk with
                                         | Loaded v -> span [ Class "version" ] [ str v ] 
                                         | Unloaded | Loading ->  div [ Class "loading" ] [ ]
                                         | Error ex -> 
                                             span [ Class "error-symbol"
                                                    Title (sprintf "Click to try again. Error details: %s" ex.Message)
                                                    OnClick (fun _ -> dispatch (LoadChannel latestReleaseChannel.Index.ReleasesJson)) ] 
                                                  [ str "×" ] )
                                       span [ Class "label" ]
                                            [ str "Latest SDK" ] ] ] ]
                  section [ Id "search"
                            Class "container" ]
                          [ Search.view m.Search (Search >> dispatch) ]
                  section [ Id "releases"
                            Class "container" ]
                          [ h2 [ ] [ str "Releases" ]
                            releasesTable dispatch m channels ]
                  section [ Id "faq"
                            Class "container" ]
                          [ h2 [ ] [ str "What version do I have?" ]
                            p [ ] 
                              [ str "You can check your version of the .NET Core runtimes and SDKs installed on your machine by running the "
                                code [ ] [ str "dotnet --info" ]
                                str " command. At the top it will show you what version of the SDK is currently in use and some information about your machine, then it will list all SDKs and runtimes that are installed. You can choose which SDK is used by changing a "
                                em [ ] [ str "global.json" ]
                                str " file, as is explained " 
                                a [ Href "https://docs.microsoft.com/en-us/dotnet/core/tools/global-json" ] [ str "here in the documentation" ] 
                                str ". You can change the runtime that is used for any application by changing the "
                                code [ ] [ str "TargetFramework" ]
                                str " in the "
                                em [ ] [ str ".(cs|fs|vb)proj" ]
                                str " file." ] ]
                  footer [ Class "container" ]
                         [ //hr [ ]
                           span [ ] [ str "© Arthur Rump, Licensed under the "
                                      a [ Href "https://github.com/arthurrump/versionsof.net/blob/master/LICENSE" ]
                                        [ str "BSD 2-Clause License" ]
                                      br [ ]
                                      str "Built with "
                                      a [ Href "https://fsharp.org" ] [ str "F#" ]
                                      str ", "
                                      a [ Href "http://fable.io" ] [ str "Fable" ]
                                      str " and "
                                      a [ Href "https://elmish.github.io" ] [ str "Elmish" ]
                                      str " - Check it out on "
                                      a [ Href "https://github.com/arthurrump/versionsof.net" ]
                                        [ str "GitHub" ] ] ] ]

    // App
    Program.mkProgram init update view
    |> Program.withReact "elmish-app"
    #if DEBUG
    |> Program.withConsoleTrace
    #endif
    |> Program.run
