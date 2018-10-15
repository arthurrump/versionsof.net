namespace VersionsOfDotNet

open Elmish
open Elmish.React
open Fable.Helpers.React
open Fable.Helpers.React.Props
open Data
open Fable.PowerPack
open System
open System.IO

type Loadable<'t> =
    | Unloaded
    | Loading
    | Error of exn
    | Loaded of 't

    static member map f (x: Loadable<'t>) =
        match x with
        | Unloaded -> Unloaded | Loading -> Loading | Error e -> Error e
        | Loaded t -> Loaded (f t)

type LazyChannelInfo =
    { LatestReleaseDate: DateTime
      LifecyclePolicy: Url
      Releases: Release list }

type ChannelModel =
    { Index: IndexEntry
      Info: Loadable<LazyChannelInfo> }

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

    let init () = Loading, fetchIndexCmd

    let yyyymmdd (date: DateTime) = date.ToString("yyyy-MM-dd")

    let update (msg:Msg) (model:Model) =
        match msg with
        | LoadIndex ->
            Loading, fetchIndexCmd
        | LoadChannel url -> 
            model |> Loadable.map (setChannelInfoFor url Loading), fetchChannelCmd url
        | FetchedIndex indices -> 
            let latestChannelUrl = 
                indices 
                |> List.maxBy (fun i -> i.ChannelVersion)
                |> fun i -> i.ReleasesJson
            let channels = indices |> List.map (fun i -> { Index = i; Info = Unloaded })
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

    let view (model:Model) dispatch =
        match model with
        | Unloaded | Loading -> 
            div [ Class "main-loading" ] 
                [ div [ Class "loading" ] [ ] ]
        | Error ex ->
            div [ Class "main-error" ]
                [ div [ Class "container column" ]
                      [ span [ Class "error-symbol" ] 
                             [ str "×" ]
                        span [ Class "error-title" ] 
                             [ str "An error occurred." ]
                        button [ Class "error-retry-button"
                                 OnClick (fun _ -> dispatch LoadIndex) ] 
                               [ str "Try again" ]
                        span [ Class "error-details" ]
                             [ str (sprintf "Details: %s" ex.Message) ] ] ]
        | Loaded channels ->
            let latestReleaseChannel = channels |> List.maxBy (fun c -> c.Index.LatestRelease)
            let latestRelease = latestReleaseChannel.Index.LatestRelease
            let latestSdk = 
                latestReleaseChannel.Info 
                |> Loadable.map (fun i -> i.Releases 
                                          |> List.maxBy (fun r -> r.Sdk.Version)
                                          |> fun r -> match r.Sdk.VersionDisplay with
                                                      | Some v -> v
                                                      | None -> r.Sdk.Version)
            div [ ]
                [ nav [ ]
                      [ div [ Class "container" ]
                            [ h1 [ Id "title" ] [ str "Versions of" ]
                              a [ Href "#core"; Class "active" ] [ str ".NET Core" ]
                              //a [ Href "#framework" ] [ str ".NET Framework" ]
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
                                          [ th [ ] [ str "Channel" ]
                                            th [ ] [ str "Latest release" ]
                                            th [ ] [ str "Support" ]
                                            th [ ] [ str "End of life date" ] ] 
                                    tbody [ ]
                                          [ for c in channels ->
                                                let i = c.Index
                                                tr [ ]
                                                   [ td [ ] [ str i.ChannelVersion ]
                                                     td [ ] [ str i.LatestRelease ]
                                                     td [ ] [ str i.SupportPhase ]
                                                     td [ ] [ str ( match i.EolDate with
                                                                    | Some d -> yyyymmdd d
                                                                    | None -> "-" ) ] ] ] ] ] ]

    // App
    Program.mkProgram init update view
    |> Program.withReact "elmish-app"
    #if DEBUG
    |> Program.withConsoleTrace
    #endif
    |> Program.run
