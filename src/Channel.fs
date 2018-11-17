namespace VersionsOfDotNet

open Data
open Elmish
open Fable.Helpers.React
open Fable.Helpers.React.Props
open System

module Channel =
    type Info =
        { LifecyclePolicy: Url
          Releases: Release.State list }

    type State =
        { Index: IndexEntry
          Info: Loadable<Info>
          Expanded: bool
          Guid: Guid }

    type Msg =
        | Load
        | Fetched of Data.Channel
        | FetchError of exn
        | Expand
        | Collapse
        | ReleaseMsg of Guid * Release.Msg

    let releaseMsg state msg = ReleaseMsg (state, msg)

    let fetchChannelCmd url = 
        Cmd.ofPromise Fetch.channel url Fetched FetchError

    let init index = 
        { Index = index
          Info = Unloaded
          Expanded = false
          Guid = Guid.NewGuid() }, Cmd.none

    let update msg state =
        match msg with
        | Load -> 
            { state with Info = Loading }, fetchChannelCmd state.Index.ReleasesJson
        | Fetched channel -> 
            let releases =
                channel.Releases
                |> List.map Release.init
            let info =
                { LifecyclePolicy = channel.LifecyclePolicy 
                  Releases = releases |> List.map fst }
            let cmds = 
                releases
                |> List.map (fun (state, cmd) -> Cmd.map (releaseMsg state.Guid) cmd)
                |> Cmd.batch
            { state with Info = Loaded info }, cmds
        | FetchError exn ->
            { state with Info = Error exn }, Cmd.none
        | Expand ->
            { state with Expanded = true }, 
            if not (Loadable.isLoaded state.Info) 
            then Cmd.ofMsg Load
            else Cmd.none
        | Collapse ->
            { state with Expanded = false }, Cmd.none
        | ReleaseMsg (guid, msg) ->
            let updateReleaseState info =
                let updatedReleases, cmds =
                    info.Releases 
                    |> List.map (fun rel -> if rel.Guid = guid 
                                            then Release.update msg rel 
                                            else rel, Cmd.none)
                    |> List.unzip
                { info with Releases = updatedReleases }, cmds |> Cmd.batch
            let info, cmd = state.Info |> Loadable.map updateReleaseState |> Loadable.unzip
            { state with 
                Info = info },
            match cmd with Loaded cmd -> Cmd.map (releaseMsg guid) cmd | _ -> Cmd.none

    let headRow = 
        tr [ ]
           [ th [ Class "hide-border" ] [ ]
             th [ ] [ ]
             th [ ] [ str "Channel" ]
             th [ ] [ str "Latest release" ]
             th [ ] [ str "Latest release date" ]
             th [ ] [ str "Support" ]
             th [ ] [ str "End of Life date" ] ]

    let view last state dispatch =
        let index = state.Index

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

        let channelRow =
            let toggleExpand _ =
                (if state.Expanded then Collapse else Expand) |> dispatch
            tr [ OnClick toggleExpand ]
               [ td [ Class "expand-button"
                      ColSpan 2 ] 
                    [ (if state.Expanded then View.chevronDown else View.chevronRight) [ ] ]
                 td [ ] [ str <| string index.ChannelVersion ]
                 td [ ] [ str <| string index.LatestRelease ]
                 td [ ] [ View.dateToHtmlTime index.LatestReleaseDate ]
                 td [ ] [ div [ Class "status-box" ] ( supportIndicator index.SupportPhase ) ]
                 td [ ] [ ( match index.EolDate with
                            | Some d -> View.dateToHtmlTime d
                            | None -> str "-" ) ] ]

        let expandedChannel () =
            match state.Info with
            | Unloaded | Loading -> 
                [ tr [ ] 
                     [ td [ ColSpan 7 ] 
                          [ div [ Class "expanded-loading" ]
                                [ div [ Class "loading" ] [ ] ] ] ] ]
            | Error ex -> 
                [ tr [ ] 
                     [ td [ ColSpan 7 ] 
                          [ div [ Class "channel-error column" ] 
                                ( View.errorView ex (fun _ -> dispatch Load) ) ] ] ]
            | Loaded info -> 
                [ tr [ ]
                     [ th [ ] [ ]
                       th [ ] [ ]
                       th [ ] [ str "Version" ]
                       th [ ] [ str "Release date" ]
                       th [ ] [ str "Runtime" ]
                       th [ ] [ str "Sdk" ]
                       th [ ] [ ] ] ] @
                [ for r in info.Releases do
                      yield! Release.view r (releaseMsg r.Guid >> dispatch) ] @
                if last then [ ] else [ headRow ]
        
        [ yield channelRow
          if state.Expanded then yield! expandedChannel () ]