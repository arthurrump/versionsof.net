namespace VersionsOfDotNet

open Data
open Elmish
open Search
open Fable.Helpers.React
open Fable.Helpers.React.Props

module ChannelsTable =
    type State = 
        { Channels: Loadable<Channel.State list>
          Filter: Search.Filter }

    type Msg =
        | Load
        | Filter of Search.Filter
        | Fetched of IndexEntry list
        | FetchError of exn
        | ChannelMsg of Channel.State * Channel.Msg

    let channelMsg state msg = ChannelMsg (state, msg)

    let fetchIndexCmd = 
        Cmd.ofPromise Fetch.index () Fetched FetchError

    let init () =
        { Channels = Unloaded
          Filter = ShowAll }, 
        Cmd.ofMsg Load

    let update msg state =
        match msg with
        | Load ->
            { state with Channels = Loading }, fetchIndexCmd
        | Filter filter ->
            { state with Filter = filter }, Cmd.none
        | Fetched index ->
            let channels =
                index 
                |> List.map Channel.init
            let states = channels |> List.map fst
            let cmds =
                channels
                |> List.map (fun (state, cmd) -> Cmd.map (channelMsg state) cmd)
                |> Cmd.batch
            { state with Channels = Loaded states }, cmds
        | FetchError ex ->
            { state with Channels = Error ex }, Cmd.none
        | ChannelMsg (channelState, msg) ->
            let newState, cmd = Channel.update msg channelState
            let newChannels channels =
                channels 
                |> List.map (fun channel ->
                                 if channel = channelState
                                 then newState else channel)
            let loadableChannels = state.Channels |> Loadable.map newChannels
            { state with Channels = loadableChannels },
            Cmd.map (channelMsg newState) cmd

    let view state dispatch =
        match state.Channels with
        | Unloaded | Loading ->
            div [ Class "main-loading" ]
                [ div [ Class "loading" ] [ ] ]
        | Error ex ->
            div [ Class "main-error" ]
                [ div [ Class "container column" ]
                      ( View.errorView ex (fun _ -> dispatch Load) ) ]
        | Loaded channels ->
            let releaseFilter filter (release: Release.State) =
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

            let filterChannels filter channels =
                match filter with
                | ShowAll -> channels
                | filter -> 
                    let filterReleases (channelModel: Channel.State) =
                        let newInfo = 
                            channelModel.Info 
                            |> Loadable.map (fun info -> 
                                { info with Releases = List.filter (releaseFilter filter) info.Releases })
                        { channelModel with Info = newInfo }

                    let filterChannelModel (channelModel: Channel.State) = 
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
                       |> List.map (fun c -> ChannelMsg (c, Channel.Load))
                       |> List.iter dispatch

                    filtered

            let filtered = filterChannels state.Filter channels

            if filtered |> List.isEmpty then
                div [ Class "channel-error column" ] 
                    [ span [ Class "error-symbol" ] 
                           [ str "×" ]
                      span [ Class "error-title" ] 
                           [ str "No releases found." ] ]
            else 
                table [ ]       
                      [ thead [ ]
                              [ Channel.headRow ]
                        tbody [ ]
                              [ for c in channels do
                                    yield! Channel.view (c = List.last channels) c (channelMsg c >> dispatch) ] ]
