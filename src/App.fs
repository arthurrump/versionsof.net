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

    member x.isLoaded =
        match x with
        | Loaded _ -> true
        | _ -> false

    static member map f (x: Loadable<'t>) =
        match x with
        | Unloaded -> Unloaded | Loading -> Loading | Error e -> Error e
        | Loaded t -> Loaded (f t)

type ChannelInfo =
    { LatestReleaseDate: DateTime
      LifecyclePolicy: Url
      Releases: Release list }

type ChannelModel =
    { Index: IndexEntry
      Info: Loadable<ChannelInfo>
      Expanded: bool }

type Model = Loadable<ChannelModel list>

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

    let init () = Loading, fetchIndexCmd

    let latestNonPreviewIndexEntry : (IndexEntry list -> IndexEntry) = 
        List.filter (fun i -> i.SupportPhase <> "preview")
        >> List.maxBy (fun i -> i.ChannelVersion)

    let latestNonPreviewChannel = 
        List.filter (fun i -> i.Index.SupportPhase <> "preview")
        >> List.maxBy (fun i -> i.Index.ChannelVersion)

    let update (msg:Msg) (model:Model) =
        match msg with
        | LoadIndex ->
            Loading, fetchIndexCmd
        | LoadChannel url -> 
            model |> Loadable.map (setChannelInfoFor url Loading), fetchChannelCmd url
        | FetchedIndex indices -> 
            let latestChannelUrl = 
                indices 
                |> latestNonPreviewIndexEntry
                |> fun i -> i.ReleasesJson
            let channels = indices |> List.map (fun i -> { Index = i; Info = Unloaded; Expanded = false })
            Loaded channels, Cmd.ofMsg (LoadChannel latestChannelUrl)
        | FetchedChannel (url, channel) -> 
            let info = { LatestReleaseDate = channel.LatestReleaseDate
                         LifecyclePolicy = channel.LifecyclePolicy
                         Releases = channel.Releases }
            model |> Loadable.map (setChannelInfoFor url (Loaded info)), Cmd.none
        | FetchError (file, ex) -> 
            match file with
            | Index -> Error ex, Cmd.none
            | Channel url ->
                model |> Loadable.map (setChannelInfoFor url (Error ex)), Cmd.none
        | ExpandChannel url ->
            let loaded = 
                match model with
                | Loaded cs ->
                    match cs |> List.tryFind (fun c -> c.Index.ReleasesJson = url) with
                    | Some c -> c.Info.isLoaded
                    | None -> false
                | _ -> false

            model |> Loadable.map (setExpandedFor url true), 
            if loaded then Cmd.none else Cmd.ofMsg (LoadChannel url)
        | CollapseChannel url ->
            model |> Loadable.map (setExpandedFor url false), Cmd.none

    let dateToHtmlTime (date: DateTime) = 
        let s = date.ToString("yyyy-MM-dd")
        time [ Props.DateTime s ] [ str s ]

    let chevronRight = img [ Src "data:image/svg+xml;base64,PD94bWwgdmVyc2lvbj0iMS4wIiA/PjwhRE9DVFlQRSBzdmcgIFBVQkxJQyAnLS8vVzNDLy9EVEQgU1ZHIDEuMS8vRU4nICAnaHR0cDovL3d3dy53My5vcmcvR3JhcGhpY3MvU1ZHLzEuMS9EVEQvc3ZnMTEuZHRkJz48c3ZnIGhlaWdodD0iNTEycHgiIGlkPSJMYXllcl8xIiBzdHlsZT0iZW5hYmxlLWJhY2tncm91bmQ6bmV3IDAgMCA1MTIgNTEyOyIgdmVyc2lvbj0iMS4xIiB2aWV3Qm94PSIwIDAgNTEyIDUxMiIgd2lkdGg9IjUxMnB4IiB4bWw6c3BhY2U9InByZXNlcnZlIiB4bWxucz0iaHR0cDovL3d3dy53My5vcmcvMjAwMC9zdmciIHhtbG5zOnhsaW5rPSJodHRwOi8vd3d3LnczLm9yZy8xOTk5L3hsaW5rIj48cGF0aCBkPSJNMjk4LjMsMjU2TDI5OC4zLDI1NkwyOTguMywyNTZMMTMxLjEsODEuOWMtNC4yLTQuMy00LjEtMTEuNCwwLjItMTUuOGwyOS45LTMwLjZjNC4zLTQuNCwxMS4zLTQuNSwxNS41LTAuMmwyMDQuMiwyMTIuNyAgYzIuMiwyLjIsMy4yLDUuMiwzLDguMWMwLjEsMy0wLjksNS45LTMsOC4xTDE3Ni43LDQ3Ni44Yy00LjIsNC4zLTExLjIsNC4yLTE1LjUtMC4yTDEzMS4zLDQ0NmMtNC4zLTQuNC00LjQtMTEuNS0wLjItMTUuOCAgTDI5OC4zLDI1NnoiLz48L3N2Zz4=" ]
    let chevronDown = img [ Src "data:image/svg+xml;base64,PD94bWwgdmVyc2lvbj0iMS4wIiA/PjwhRE9DVFlQRSBzdmcgIFBVQkxJQyAnLS8vVzNDLy9EVEQgU1ZHIDEuMS8vRU4nICAnaHR0cDovL3d3dy53My5vcmcvR3JhcGhpY3MvU1ZHLzEuMS9EVEQvc3ZnMTEuZHRkJz48c3ZnIGhlaWdodD0iNTEycHgiIGlkPSJMYXllcl8xIiBzdHlsZT0iZW5hYmxlLWJhY2tncm91bmQ6bmV3IDAgMCA1MTIgNTEyOyIgdmVyc2lvbj0iMS4xIiB2aWV3Qm94PSIwIDAgNTEyIDUxMiIgd2lkdGg9IjUxMnB4IiB4bWw6c3BhY2U9InByZXNlcnZlIiB4bWxucz0iaHR0cDovL3d3dy53My5vcmcvMjAwMC9zdmciIHhtbG5zOnhsaW5rPSJodHRwOi8vd3d3LnczLm9yZy8xOTk5L3hsaW5rIj48cGF0aCBkPSJNMjU2LDI5OC4zTDI1NiwyOTguM0wyNTYsMjk4LjNsMTc0LjItMTY3LjJjNC4zLTQuMiwxMS40LTQuMSwxNS44LDAuMmwzMC42LDI5LjljNC40LDQuMyw0LjUsMTEuMywwLjIsMTUuNUwyNjQuMSwzODAuOSAgYy0yLjIsMi4yLTUuMiwzLjItOC4xLDNjLTMsMC4xLTUuOS0wLjktOC4xLTNMMzUuMiwxNzYuN2MtNC4zLTQuMi00LjItMTEuMiwwLjItMTUuNUw2NiwxMzEuM2M0LjQtNC4zLDExLjUtNC40LDE1LjgtMC4yTDI1NiwyOTguMyAgeiIvPjwvc3ZnPg==" ]

    let supportIndicator supportPhase =
        match supportPhase with
        | "preview" ->
            [ span [ Class "support-indicator preview"
                     Title "Preview" ] [ ]
              span [ ] [ str "Preview" ] ]
        | "lts" -> 
            [ span [ Class "support-indicator lts"
                     Title "Long Term Support" ] [ ]
              span [ ] [ str "Long Term Support" ] ]
        | "eol" -> 
            [ span [ Class "support-indicator eol"
                     Title "End of Life" ] [ ]
              span [ ] [ str "End of Life" ] ]
        | "maintenance" -> 
            [ span [ Class "support-indicator maintenance"
                     Title "Maintenance" ] [ ]
              span [ ] [ str "Maintenance" ] ]
        | t -> 
            [ span [ Class "support-indicator unknown" 
                     Title t ] [ str "?" ]
              span [ ] [ str t ] ]

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
             th [ ] [ str "Support" ]
             th [ ] [ str "End of Life date" ] ]

    let channelRow dispatch c =
        let i = c.Index
        tr [ OnClick (fun _ -> dispatch (if c.Expanded
                                         then CollapseChannel i.ReleasesJson
                                         else ExpandChannel i.ReleasesJson)) ]
           [ td [ Class "expand-button"
                  ColSpan 2.0 ] 
                [ (if c.Expanded then chevronDown else chevronRight) ]
             td [ ] [ str i.ChannelVersion ]
             td [ ] [ str i.LatestRelease ]
             td [ ] [ div [ Class "support-box" ] ( supportIndicator i.SupportPhase ) ]
             td [ ] [ ( match i.EolDate with
                        | Some d -> dateToHtmlTime d
                        | None -> str "-" ) ] ]

    let expandedChannel dispatch c last =
        match c.Info with
        | Unloaded | Loading -> 
            [ tr [ ] 
                 [ td [ ColSpan 6.0 ] 
                      [ div [ Class "expanded-loading" ]
                            [ div [ Class "loading" ] [ ] ] ] ] ]
        | Error ex -> 
            [ tr [ ] 
                 [ td [ ColSpan 6.0 ] 
                      [ div [ Class "channel-error column" ] 
                            ( errorView ex (fun _ -> dispatch (LoadChannel c.Index.ReleasesJson)) ) ] ] ]
        | Loaded info -> 
            [ tr [ ]
                 [ th [ ] [ ]
                   th [ ] [ ]
                   th [ ] [ str "Release date" ]
                   th [ ] [ str "Release Version" ]
                   th [ ] [ str "Runtime" ]
                   th [ ] [ str "Sdk" ] ] ] @
            [ for r in info.Releases ->
                  tr [ ]
                     [ td [ Class "hide-border" ] [ ]
                       td [ Class "expand-button" ] [ (*chevronRight*) ]
                       td [ ] [ dateToHtmlTime r.ReleaseDate ]
                       td [ ] [ str (Option.defaultValue "-" r.ReleaseVersion) ]
                       td [ ] [ str (match r.Runtime with
                                     | Some r -> Option.defaultValue r.Version r.VersionDisplay
                                     | None -> "-") ]
                       td [ ] [ str (Option.defaultValue r.Sdk.Version r.Sdk.VersionDisplay) ] ] ] @
            if last then [ ] else [ headRow ]

    let view (model:Model) dispatch =
        match model with
        | Unloaded | Loading -> 
            div [ Class "main-loading" ] 
                [ div [ Class "loading" ] [ ] ]
        | Error ex ->
            div [ Class "main-error" ]
                [ div [ Class "container column" ]
                      ( errorView ex (fun _ -> dispatch LoadIndex) ) ]
        | Loaded channels ->
            let latestReleaseChannel = channels |> latestNonPreviewChannel
            let latestRelease = latestReleaseChannel.Index.LatestRelease
            let latestSdk = 
                latestReleaseChannel.Info 
                |> Loadable.map (fun i -> i.Releases 
                                          |> List.maxBy (fun r -> r.ReleaseDate)
                                          |> fun r -> match r.Sdk.VersionDisplay with
                                                      | Some v -> v
                                                      | None -> r.Sdk.Version)
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
                               [ div [ Class "cell column" ]
                                     [ span [ Class "version" ] 
                                            [ str latestRelease ]
                                       span [ Class "label" ] 
                                            [ str "Latest runtime" ] ]
                                 div [ Class "cell column" ]
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
