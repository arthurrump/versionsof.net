module App

open Elmish
open Elmish.React
open Fable.Helpers.React
open Fable.Helpers.React.Props
open Fable.PowerPack
open Fable.PowerPack.Fetch
open System
open Thoth.Json

let [<Literal>] ReleasesIndexUrl = "https://raw.githubusercontent.com/dotnet/core/master/release-notes/releases-index.json"

type Url = string
type Version = string

let getOptionalDate (get: Decode.IGetters) jsonName =
    get.Required.Field jsonName Decode.string
    |> fun s -> if String.IsNullOrWhiteSpace s then (false, DateTime()) else DateTime.TryParse(s)
    |> fun (s, d) -> if s then Some d else None

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
                  EolDate = getOptionalDate get "eol-date"
                  ReleasesJson = get.Required.Field "releases.json" Decode.string })

type File =
    { Name: string
      Url: Url
      Hash: string }

    static member Decoder : Decode.Decoder<File> =
        Decode.object
            (fun get ->
                { Name = get.Required.Field "name" Decode.string
                  Url = get.Required.Field  "url" Decode.string
                  Hash = get.Required.Field "hash" Decode.string })

type Runtime =
    { Version: Version
      VersionDisplay: Version option
      VsVersion: Version option
      Files: File list }

    static member Decoder : Decode.Decoder<Runtime> =
        Decode.object
            (fun get ->
                { Version = get.Required.Field "version" Decode.string
                  VersionDisplay = get.Optional.Field "version-display" Decode.string
                  VsVersion = get.Optional.Field "vs-version" Decode.string
                  Files = get.Required.Field "files" (Decode.list File.Decoder) })

type Sdk = 
    { Version: Version
      VersionDisplay: Version option
      VsVersion: Version option
      CsharpLanguage: Version option
      FsharpLanguage: Version option
      Files: File list }

    static member Decoder : Decode.Decoder<Sdk> =
        Decode.object
            (fun get ->
                { Version = match get.Optional.Field "version" Decode.string with
                            | Some v -> v
                            | None -> get.Required.Field "version-sdk" Decode.string
                  VersionDisplay = get.Optional.Field "version-display" Decode.string
                  VsVersion = get.Optional.Field "vs-version" Decode.string
                  CsharpLanguage = get.Optional.Field "csharp-language" Decode.string
                  FsharpLanguage = get.Optional.Field "fsharp-language" Decode.string
                  Files = get.Required.Field "files" (Decode.list File.Decoder) })

type AspnetcoreRuntime =
    { Version: Version
      VersionDisplay: Version option
      VersionAspnetcoremodule: Version list option
      Files: File list }

    static member Decoder : Decode.Decoder<AspnetcoreRuntime> =
        Decode.object
            (fun get ->
                { Version = get.Required.Field "version" Decode.string
                  VersionDisplay = get.Optional.Field "version-display" Decode.string
                  VersionAspnetcoremodule = get.Optional.Field "version-aspnetcoremodule" (Decode.list Decode.string)
                  Files = get.Required.Field "files" (Decode.list File.Decoder) })

type Symbols =
    { Version: Version
      Files: File list }

    static member Decoder : Decode.Decoder<Symbols> =
        Decode.object
            (fun get ->
                { Version = get.Required.Field "version" Decode.string
                  Files = get.Required.Field "files" (Decode.list File.Decoder) })

type Release = 
    { ReleaseDate: DateTime
      ReleaseVersion: Version option
      Security: bool
      ReleaseNotes: Url option
      Runtime: Runtime option
      Sdk: Sdk
      AspnetcoreRuntime: AspnetcoreRuntime option
      Symbols: Symbols option }

    static member Decoder : Decode.Decoder<Release> =
        Decode.object
            (fun get -> 
                { ReleaseDate = get.Required.Field "release-date" Decode.datetime
                  ReleaseVersion = get.Optional.Field "release-version" Decode.string
                  Security = get.Required.Field "security" Decode.bool
                  ReleaseNotes = get.Optional.Field "release-notes" Decode.string
                  Runtime = get.Optional.Field "runtime" Runtime.Decoder
                  Sdk = get.Required.Field "sdk" Sdk.Decoder
                  AspnetcoreRuntime = get.Optional.Field "aspnetcore-runtime" AspnetcoreRuntime.Decoder
                  Symbols = get.Optional.Field "symbols" Symbols.Decoder })

type Channel =
    { ChannelVersion: Version
      LatestRelease: Version
      LatestReleaseDate: DateTime
      SupportPhase: string
      EolDate: DateTime option
      LifecyclePolicy: Url
      Releases: Release list }

    static member Decoder : Decode.Decoder<Channel> =
        Decode.object
            (fun get ->
                { ChannelVersion = get.Required.Field "channel-version" Decode.string
                  LatestRelease = get.Required.Field "latest-release" Decode.string
                  LatestReleaseDate = get.Required.Field "latest-release-date" Decode.datetime
                  SupportPhase = get.Required.Field "support-phase" Decode.string
                  EolDate = getOptionalDate get "eol-date"
                  LifecyclePolicy = get.Required.Field "lifecycle-policy" Decode.string
                  Releases = get.Required.Field "releases" (Decode.list Release.Decoder) })

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
    | LoadChannel of Url
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

let loadChannel (githubUrl: Url) =
    promise {
        let url = githubUrl.Replace("github.com/dotnet/core/blob", "raw.githubusercontent.com/dotnet/core")
        let! response = fetch url []
        let! json = response.text()
        match Decode.fromString Channel.Decoder json with
        | Ok res -> return ChannelLoaded res
        | Error ex -> return LoadingError (Exception ex)
    }

let loadChannelCmd githubUrl =
    Cmd.ofPromise loadChannel
                  githubUrl
                  (fun msg -> msg)
                  (fun ex -> LoadingError ex)

let init () = Loading, loadIndexCmd

let update (msg:Msg) (model:Model) =
    match msg with
    | LoadingError _ -> model, Cmd.none // TODO: exponential backoff with limit
    | IndexLoaded indexes -> 
        let latestChannel = indexes |> List.maxBy (fun i -> i.ChannelVersion)
        let channelUrl = latestChannel.ReleasesJson
        Loaded (indexes |> List.map (fun i -> i, Unloaded)), loadChannelCmd channelUrl
    | LoadChannel url -> 
        match model with
        | Loading -> Loading, Cmd.none
        | Loaded indexes -> 
            let m = 
                indexes 
                |> List.map (fun (i, cw) -> if i.ReleasesJson = url 
                                            then i, ChannelModel.Loading 
                                            else i, cw)
            Loaded m, loadChannelCmd url
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
                          (match cm with
                           | ChannelModel.Loaded c -> str (sprintf "Sdk: %s" c.Releases.Head.Sdk.Version)
                           | ChannelModel.Loading -> str "Loading Sdk..."
                           | ChannelModel.Unloaded -> button [ OnClick (fun _ -> dispatch (LoadChannel i.ReleasesJson)) ] [ str "Load Sdk" ])
                          br []
                          (match i.EolDate with
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
