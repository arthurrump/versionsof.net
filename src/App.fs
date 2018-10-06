module App

open Elmish
open Elmish.React
open Fable.Helpers.React
open Fable.Helpers.React.Props
open Fable.PowerPack
open Fable.PowerPack.Date.Local
open Fable.PowerPack.Fetch
open System
open Thoth.Json

let [<Literal>] ReleasesIndexUrl = "https://raw.githubusercontent.com/dotnet/core/master/release-notes/releases-index.json"

type Url = string
type Version = string

type IndexRelease =
    { ChannelVersion: Version
      LatestRelease: Version
      Product: string
      SupportPhase: string
      EolDate: DateTime option
      ReleasesJson: Url }

    static member Decoder : Decode.Decoder<IndexRelease> = 
        Decode.object
            (fun get -> 
                { ChannelVersion = get.Required.Field "channel-version" Decode.string
                  LatestRelease = get.Required.Field "latest-release" Decode.string
                  Product = get.Required.Field "product" Decode.string
                  SupportPhase = get.Required.Field "support-phase" Decode.string
                  EolDate = get.Required.Field "eol-date" Decode.string
                            |> fun s -> if String.IsNullOrWhiteSpace s then (false, DateTime()) else DateTime.TryParse(s)
                            |> fun (s, d) -> if s then Some d else None
                  ReleasesJson = get.Required.Field "releases.json" Decode.string })

type File =
    { Name: string
      Url: Url
      Hash: string }

type Runtime =
    { Version: Version
      VersionDisplay: Version option
      VsVersion: Version option
      Files: File list }

type Sdk = 
    { Version: Version
      VersionDisplay: Version option
      VsVersion: Version option
      CsharpLanguage: Version option
      FsharpLanguage: Version option
      Files: File list }

type AspnetcoreRuntime =
    { Version: Version
      VersionDisplay: Version
      VersionAspnetcoremodule: Version list
      Files: File list }

type Symbol =
    { Version: Version
      Files: File list }

type Release = 
    { ReleaseDate: Date
      ReleaseVersion: Version
      Security: bool
      ReleaseNotes: Url
      Runtime: Runtime
      Sdk: Sdk
      AspnetcoreRuntime: AspnetcoreRuntime option
      Symbol: Symbol option }

type Channel =
    { ChannelVersion: Version
      LatestRelease: Version
      LatestReleaseDate: DateTime
      SupportPhase: string
      EolDate: Date option
      LifecyclePolicy: Url }

type ChannelModel =
    | Unloaded
    | Loading
    | Loaded of Channel

type Model =
    | Loading
    | Loaded of (IndexRelease * ChannelModel) list

type Msg =
    | LoadingError of Exception
    | IndexLoaded of IndexRelease list
    | ChannelLoaded of Channel

let loadIndex () =
    promise {
        let! response = fetch ReleasesIndexUrl []
        let! json = response.text()
        let decoder = Decode.object (fun get -> get.Required.Field "releases-index" (Decode.list IndexRelease.Decoder))
        match Decode.fromString decoder json with
        | Ok res -> return IndexLoaded res
        | Error ex -> return LoadingError (Exception ex)
    }

let loadIndexCmd = 
    Cmd.ofPromise loadIndex
                  ()
                  (fun msg -> msg)
                  (fun ex -> LoadingError ex)

let init () = Loading, loadIndexCmd

let update (msg:Msg) (model:Model) =
    match msg with
    | LoadingError _ -> model, Cmd.none // TODO: exponential backoff with limit
    | IndexLoaded indexes -> Loaded (indexes |> List.map (fun i -> i, Unloaded)), Cmd.none
    | ChannelLoaded channel -> 
        match model with
        | Loading -> Loading, Cmd.none
        | Loaded indexes ->
            Loaded (indexes |> List.map (fun (i, cm) -> if i.ChannelVersion = channel.ChannelVersion then
                                                           (i, ChannelModel.Loaded channel)
                                                        else
                                                           (i, cm))), Cmd.none

let view (model:Model) dispatch =
    match model with
    | Loading -> str "Loading index..."
    | Loaded icms ->
        div [] 
            [ for i, cm in icms ->
                 div [] [ str (sprintf "%s - %s" i.ChannelVersion i.LatestRelease)
                          br []
                          (match i.EolDate with
                           | Some eol -> str (Date.Format.localFormat Date.Local.german "yyyy-MM-dd" eol)
                           | None -> str "No known end of life")
                          br [] ] ]

// App
Program.mkProgram init update view
|> Program.withReact "elmish-app"
#if DEBUG
|> Program.withConsoleTrace
#endif
|> Program.run
