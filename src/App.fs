namespace VersionsOfDotNet

open Elmish
open Elmish.React
open Fable.Helpers.React
open Fable.Helpers.React.Props
open Data

type State = 
    { ChannelsTable: ChannelsTable.State 
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

        { ChannelsTable = ctState
          Search = sState },
        cmds

    let latestNonPreviewChannel : (Channel.State list -> Channel.State) = 
        List.filter (fun c -> c.Index.SupportPhase <> "preview")
        >> List.maxBy (fun c -> c.Index.ChannelVersion)

    let update msg state =
        match msg with
        | ChannelsTableMsg msg ->
            let newState, ctCmd = ChannelsTable.update msg state.ChannelsTable
            { state with ChannelsTable = newState }, Cmd.map ChannelsTableMsg ctCmd
        | SearchMsg msg -> 
            let newState, sCmd = Search.update msg state.Search
            let cmd = 
                match msg with
                | Search.FilterSet (filter, _) -> Cmd.ofMsg (ChannelsTableMsg (ChannelsTable.Filter filter))
                | _ -> Cmd.none
            { state with Search = newState }, 
            [ Cmd.map SearchMsg sCmd; cmd ] |> Cmd.batch
        
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
            let latestRuntime = string latestReleaseChannel.Index.LatestRuntime
            let latestSdk = string latestReleaseChannel.Index.LatestSdk

            div [ ]
                [ nav [ ]
                      [ div [ Class "container" ]
                            [ h1 [ Id "title" ] [ str "Versions of" ]
                              a [ Href "#core"; Class "active" ] [ str ".NET Core" ]
                              a [ (*Href "#standard"*) Disabled true; Title "Coming soon™"; Style [ Cursor "default"; CSSProp.Color "rgba(255, 255, 255, 0.6)" ] ] [ str ".NET Standard" ]
                            ] ]
                  header [ ]
                         [ div [ Class "container row" ]
                               [ div [ Class "cell" ]
                                     [ span [ Class "version" ] 
                                            [ str latestRuntime ]
                                       span [ Class "label" ] 
                                            [ str "Latest runtime" ] ]
                                 div [ Class "cell" ]
                                     [ span [ Class "version" ] 
                                            [ str latestSdk ] 
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
                                      a [ Href "https://fable.io" ] [ str "Fable" ]
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
