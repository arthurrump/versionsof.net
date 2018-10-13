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
            | Index -> 
                Error ex, Cmd.none // TODO: exponential backoff with limit
            | Channel url ->
                model |> Loadable.map (setChannelInfoFor url (Error ex)), Cmd.none

    let view (model:Model) dispatch =
        match model with
        | Unloaded | Loading -> str "Loading index..."
        | Error ex ->
            div []
                [ str (sprintf "Could not load, error: %s" ex.Message)
                  button [ OnClick (fun _ -> dispatch (LoadIndex)) ] [ str "Retry" ] ]
        | Loaded channels ->
            div [] 
                [ for c in channels ->
                     div [] [ str (sprintf "%s - %s" c.Index.ChannelVersion c.Index.LatestRelease)
                              br []
                              (match c.Info with
                               | Loaded info -> str (sprintf "Sdk: %s" info.Releases.Head.Sdk.Version)
                               | Loading -> str "Loading Sdk..."
                               | Error ex -> 
                                    div [ ]
                                        [ str (sprintf "Could not load, error: %s" ex.Message)
                                          button [ OnClick (fun _ -> dispatch (LoadChannel c.Index.ReleasesJson)) ]  [str "Retry" ] ]
                               | Unloaded -> button [ OnClick (fun _ -> dispatch (LoadChannel c.Index.ReleasesJson)) ] [ str "Load Sdk" ])
                              br []
                              (match c.Index.EolDate with
                               | Some eol -> str (sprintf "EOL: %s" (Date.Format.localFormat Date.Local.german "yyyy-MM-dd" eol))
                               | None -> str "No known end of life")
                              br []
                              br [] ] ]

    // App
    Program.mkProgram init update view
    |> Program.withReact "elmish-app"
    #if DEBUG
    |> Program.withConsoleTrace
    #endif
    |> Program.run
