namespace VersionsOfDotNet

open Elmish
open Elmish.React
open Fable.Helpers.React
open Fable.Helpers.React.Props
open Data
open Fable.PowerPack
open System

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
      SearchFocus: bool
      SearchQuery: string }

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
    | SearchFocusChanged of bool
    | SearchQueryChanged of string

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
                     SearchFocus = false
                     SearchQuery = "" }, 
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

    let channelRow dispatch c =
        let i = c.Index
        let toggleExpand _ =
            dispatch (if c.Expanded
                      then CollapseChannel i.ReleasesJson
                      else ExpandChannel i.ReleasesJson)
        tr [ OnClick toggleExpand ]
           [ td [ Class "expand-button"
                  ColSpan 2.0 ] 
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
            |> Option.exists (fun r -> r.VersionDisplay |> Option.exists (fun vd -> r.Version <> vd))

        let fullSdkVersion (sdk: Sdk) =
            sdk.VersionDisplay |> Option.exists (fun vd -> sdk.Version <> vd)

        let lif = Printf.kprintf (fun s -> li [ ] [ str s ])

        let lia href text = li [ ] [ a [ Href href ] [ str text ] ]

        let (|SomeText|_|) input = 
            match input with
            | Some i when not (String.IsNullOrWhiteSpace(i)) -> Some i
            | _ -> None

        tr [ ]
           [ td [ Class "hide-border" ] [ ]
             td [ Class "hide-border" ] [ ]
             td [ Class "hide-border"
                  ColSpan 5.0 ]
                [ ul [ Class "expanded-release" ]
                      [ if fullRuntimeVersion r.Runtime then 
                            yield lif "Runtime version %s" r.Runtime.Value.Version 
                        if fullSdkVersion r.Sdk then
                            yield lif "Sdk version %s" r.Sdk.Version
                        match r.Sdk.VsVersion with SomeText v -> yield lif "Included in Visual Studio %s" v | _ -> ()
                        match r.Sdk.CsharpLanguage with SomeText v -> yield lif "Supports C# %s" v | _ -> ()
                        match r.Sdk.FsharpLanguage with SomeText v -> yield lif "Supports F# %s" v | _ -> ()
                        match r.Sdk.VbLanguage with SomeText v -> yield lif "Supports Visual Basic %s" v | _ -> ()
                        match r.AspnetcoreRuntime with 
                        | Some a -> 
                            yield lif "ASP.NET Core Runtime %s" a.Version
                            match a.VersionAspnetcoremodule with 
                            | Some a when not a.IsEmpty -> 
                                yield lif "ASP.NET Core IIS Module %s" a.Head
                            | _ -> ()
                        | None -> ()
                        match r.ReleaseNotes with Some url -> yield lia url "Release notes" | None -> () ] ] ]

    let expandedChannel dispatch c last =
        match c.Info with
        | Unloaded | Loading -> 
            [ tr [ ] 
                 [ td [ ColSpan 7.0 ] 
                      [ div [ Class "expanded-loading" ]
                            [ div [ Class "loading" ] [ ] ] ] ] ]
        | Error ex -> 
            [ tr [ ] 
                 [ td [ ColSpan 7.0 ] 
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
                           td [ ] [ str (Option.defaultValue "-" r.ReleaseVersion) ]
                           td [ ] [ dateToHtmlTime r.ReleaseDate ]
                           td [ ] [ str (match r.Runtime with
                                         | Some r -> Option.defaultValue r.Version r.VersionDisplay
                                         | None -> "-") ]
                           td [ ] [ str (Option.defaultValue r.Sdk.Version r.Sdk.VersionDisplay) ]
                           td [ ] ( securityIndicator r ) ]
                  if rm.Expanded then yield expandedRelease rm ] @
            if last then [ ] else [ headRow ]

    let searchSuggestions m =
        let (|Release|Runtime|Sdk|AspRuntime|AspModule|Empty|Unknown|) (query: string) =
            let q = query.Trim().ToLowerInvariant()
            let qa = q.Split() |> Array.toList
            printfn "%A" qa
            match qa with
            | "rel"::qs | "release"::qs -> Release (String.concat " " qs)
            | "rt"::qs | "run"::qs | "runtime"::qs -> Runtime (String.concat " " qs)
            | "sdk"::qs -> Sdk (String.concat " " qs)
            | "asp"::qs | "aspnet"::qs | "asprt"::qs |
              "aspruntime"::qs | "aspnetrt"::qs -> AspRuntime (String .concat " " qs)
            | "aspmod"::qs | "iismod"::qs -> AspModule (String.concat " " qs)
            | [ "" ] -> printfn "Empty!"; Empty
            | _ -> printfn "Unknown!"; Unknown q

        let (|Version|_|) (input: string) =
            let i = input.Trim().ToLowerInvariant()
            let versionParts =
                i.Split('.') 
                |> Array.toList
                |> List.filter (fun i -> i <> "")
            let isVersionLastPart (last: string) =
                let ls = last.Split('-') |> Array.toList
                Int32.TryParse(ls.Head) |> fst
            let isVersion = not (List.isEmpty versionParts) && 
                            List.forall (fun i -> Int32.TryParse(i) |> fst || 
                                                  if i = List.last versionParts 
                                                  then isVersionLastPart i 
                                                  else false) versionParts
            if isVersion then Some i else None
            
        let (|StrLength|_|) length (input: string) =
            if input.Length = length then Some input else None

        let searchHints = 
            [ ("rel", "Release")
              ("rt", "Runtime")
              ("sdk", "Sdk")
              ("asp", "ASP.NET Core Runtime")
              ("aspmod", "ASP.NET Core IIS Module") ]

        let sugLi text label =
            li [ ] [ str text
                     span [ Class "label" ] [ str label ] ]

        let hintList version hints =
            hints |> List.map (fun (t, n) -> sugLi (sprintf "%s %s" t version) n)

        div [ Id "search-suggestions" ]
            [ ul [ ] 
                 ( match m.SearchQuery with
                   | Empty -> searchHints |> hintList "x.x.x"
                   | Version x -> searchHints |> hintList x
                   | StrLength 1 c -> searchHints |> List.filter (fun (s, _) -> s.StartsWith(c)) |> hintList "x.x.x"
                   | StrLength 2 c -> searchHints |> List.filter (fun (s, _) -> s.StartsWith(c)) |> hintList "x.x.x"
                   | Release (Version x) -> [ sugLi x "Release" ]
                   | Release "" -> [ sugLi "x.x.x" "Release" ]
                   | Runtime (Version x) -> [ sugLi x "Runtime" ]
                   | Runtime "" -> [ sugLi "x.x.x" "Runtime" ]
                   | Sdk (Version x) -> [ sugLi x "Sdk" ]
                   | Sdk "" -> [ sugLi "x.x.x" "Sdk"]
                   | StrLength 3 c -> searchHints |> List.filter (fun (s, _) -> s.StartsWith(c)) |> hintList "x.x.x"
                   | AspRuntime (Version x) -> [ sugLi x "ASP.NET Core Runtime" ]
                   | AspRuntime "" -> [ sugLi "x.x.x" "ASP.NET Core Runtime" ]
                   | StrLength 4 c -> searchHints |> List.filter (fun (s, _) -> s.StartsWith(c)) |> hintList "x.x.x"
                   | StrLength 5 c -> searchHints |> List.filter (fun (s, _) -> s.StartsWith(c)) |> hintList "x.x.x"
                   | AspModule (Version x) -> [ sugLi x "ASP.NET Core IIS Module" ]
                   | AspModule "" -> [ sugLi "x.x.x" "ASP.NET Core IIS Module" ]
                   | x -> [ sugLi x "Invalid version" ] ) ]

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
                                                      | None -> r.Release.Sdk.Version)
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
                          [ div [ Class "input-wrapper" ]
                                [ yield input [ Placeholder "Find a version..."
                                                Props.Type "search"
                                                OnFocus (fun _ -> dispatch (SearchFocusChanged true))
                                                OnBlur (fun _ -> dispatch (SearchFocusChanged false))
                                                OnChange (fun e -> dispatch (SearchQueryChanged e.Value))
                                                Value m.SearchQuery
                                                Class (if m.SearchFocus then "focus" else "") ]
                                  if m.SearchFocus then yield searchSuggestions m ] ]
                  section [ Id "releases"
                            Class "container" ]
                          [ h2 [ ] [ str "Releases" ]
                            table [ ]       
                                  [ thead [ ]
                                          [ headRow ]
                                    tbody [ ]
                                          ( [ for c in channels ->
                                                  [ yield channelRow dispatch c
                                                    if c.Expanded then yield! expandedChannel dispatch c (c = List.last channels) ] ] 
                                            |> List.concat ) ] ] ]

    // App
    Program.mkProgram init update view
    |> Program.withReact "elmish-app"
    #if DEBUG
    |> Program.withConsoleTrace
    #endif
    |> Program.run
