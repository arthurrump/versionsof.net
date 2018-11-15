namespace VersionsOfDotNet

open Elmish
open Elmish.React
open Fable.Helpers.React
open Fable.Helpers.React.Props
open Data

type State = 
    { LatestRuntime: Loadable<Version>
      ChannelsTable: ChannelsTable.State 
      Search: Search.State }

type File =
    | Index
    | Channel of Url

type Msg =
    | ChannelsTableMsg of ChannelsTable.Msg
    | SearchMsg of Search.Msg

module App =
    let init () = 
        let ctState, ctCmd = ChannelsTable.init ()
        let sState, sCmd = Search.init ()
        let cmds = 
            [ Cmd.map ChannelsTableMsg ctCmd
              Cmd.map SearchMsg sCmd ]
            |> Cmd.batch

        { LatestRuntime = Unloaded
          ChannelsTable = ctState
          Search = sState },
        cmds

    let latestNonPreviewChannel : (Channel.State list -> Channel.State) = 
        List.filter (fun c -> c.Index.SupportPhase <> "preview")
        >> List.maxBy (fun c -> c.Index.ChannelVersion)

    let update msg state =
        match msg with
        | ChannelsTableMsg msg ->
            let newState, ctCmd = ChannelsTable.update msg state.ChannelsTable
            let state, cmd = 
                match msg with 
                | ChannelsTable.Fetched entries ->
                    let latestChannel = 
                        entries
                        |> List.filter (fun i -> i.SupportPhase <> "preview")
                        |> List.maxBy (fun i -> i.LatestRelease)
                    { state with LatestRuntime = Loaded latestChannel.LatestRelease },
                    Cmd.ofMsg (ChannelsTable.channelMsg (Channel.init latestChannel |> fst) Channel.Load |> ChannelsTableMsg)
                | ChannelsTable.Load -> { state with LatestRuntime = Loading }, Cmd.none
                | ChannelsTable.FetchError ex -> { state with LatestRuntime = Error ex }, Cmd.none
                | _ -> state, Cmd.none
            { state with ChannelsTable = newState }, 
            [ Cmd.map ChannelsTableMsg ctCmd; cmd ] |> Cmd.batch
        | SearchMsg msg -> 
            let newState, cmd = Search.update msg state.Search
            { state with Search = newState }, Cmd.map SearchMsg cmd
        
    let view state dispatch =
        match state.ChannelsTable.Channels with
        | Unloaded | Loading -> 
            div [ Class "main-loading" ] 
                [ div [ Class "loading" ] [ ] ]
        | Error ex ->
            div [ Class "main-error" ]
                [ div [ Class "container column" ]
                      ( View.errorView ex (fun _ -> dispatch (ChannelsTableMsg ChannelsTable.Load)) ) ]
        | Loaded channels ->
            let latestReleaseChannel = channels |> latestNonPreviewChannel
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
                                            [ str <| match state.LatestRuntime with Loaded v -> string v | _ -> "" ]
                                       span [ Class "label" ] 
                                            [ str "Latest runtime" ] ]
                                 div [ Class "cell" ]
                                     [ ( match latestSdk with
                                         | Loaded v -> span [ Class "version" ] [ str v ] 
                                         | Unloaded | Loading ->  div [ Class "loading" ] [ ]
                                         | Error ex -> 
                                             span [ Class "error-symbol"
                                                    Title (sprintf "Click to try again. Error details: %s" ex.Message)
                                                    OnClick (fun _ -> dispatch (ChannelsTableMsg <| ChannelsTable.channelMsg latestReleaseChannel Channel.Load)) ] 
                                                  [ str "×" ] )
                                       span [ Class "label" ]
                                            [ str "Latest SDK" ] ] ] ]
                  section [ Id "search"
                            Class "container" ]
                          [ Search.view state.Search (SearchMsg >> dispatch) ]
                  section [ Id "releases"
                            Class "container" ]
                          [ h2 [ ] [ str "Releases" ]
                            ChannelsTable.view state.ChannelsTable (ChannelsTableMsg >> dispatch) ]
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
                           span [ ] [ str "© "
                                      a [ Href "https://arthurrump.com" ]
                                        [ str "Arthur Rump" ]
                                      str ", Licensed under the "
                                      a [ Href "https://github.com/arthurrump/versionsof.net/blob/master/LICENSE" ]
                                        [ str "BSD 2-Clause License" ]
                                      br [ ]
                                      str "Built with "
                                      a [ Href "https://fsharp.org" ] [ str "F#" ]
                                      str ", "
                                      a [ Href "http://fable.io" ] [ str "Fable" ] // DevSkim: ignore DS137138 
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
