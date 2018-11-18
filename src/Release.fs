namespace VersionsOfDotNet

open Data
open Fable.Helpers.React
open Fable.Helpers.React.Props
open Elmish
open Fable.Core
open System
open Fable.Import.React

module Release =
    type State =
        { Release: Release
          Expanded: bool
          ReleaseNotes: Loadable<ReactElement>
          Guid: Guid }

    type Msg =
        | Expand
        | Collapse
        | LoadReleaseNotes
        | FetchedReleaseNotes of string
        | FetchError of exn

    let init rel = 
        { Release = rel 
          Expanded = false
          ReleaseNotes = Unloaded
          Guid = Guid.NewGuid() }, Cmd.none

    let loadReleaseNotesCmd url =
        Cmd.ofPromise Fetch.releaseNotes url FetchedReleaseNotes FetchError

    let update msg state =
        match msg with
        | Expand -> 
            { state with Expanded = true },
            if state.ReleaseNotes |> Loadable.isLoaded 
            then Cmd.none else Cmd.ofMsg LoadReleaseNotes
        | Collapse -> { state with Expanded = false }, Cmd.none
        | LoadReleaseNotes ->
            { state with ReleaseNotes = Loading }, 
            match state.Release.ReleaseNotes with Some url -> loadReleaseNotesCmd url | None -> Cmd.none
        | FetchedReleaseNotes text ->
            let text = text.Replace("\|", "/") // In the download tables a pipe is used to separate download links,
                                               // but the escaping is not parsed correctly
            let elem = Import.MarkdownToJsx.MarkdownToJsx.compiler text (JsInterop.createObj [])
            { state with ReleaseNotes = Loaded elem }, Cmd.none
        | FetchError ex ->
            { state with ReleaseNotes = Error ex }, Cmd.none

    let view state dispatch =
        let r = state.Release

        let securityIndicator = 
            if r.Security then
                [ div [ Class "status-box" ]
                      [ span [ Class "status-indicator security"
                               Title "Security" ] [ str "!" ]
                        span [ ] [ str "Security" ] ] ]
            else [ ]

        let expandedRelease () =
            let fullRuntimeVersion (runtime: Runtime option) =
                runtime
                |> Option.exists (fun r -> r.VersionDisplay 
                                           |> Option.exists (fun vd -> r.Version |> Version.displayedAs vd))

            let fullSdkVersion (sdk: Sdk) =
                sdk.VersionDisplay |> Option.exists (fun vd -> sdk.Version |> Version.displayedAs vd)

            let lif fmt = Printf.kprintf (fun s -> li [ ] [ str s ]) fmt

            let lia href text = li [ ] [ a [ Href href ] [ str text ] ]

            let (|SomeText|_|) input = 
                match input with
                | Some i when not (String.isWhitespace i) -> Some i
                | _ -> None

            tr [ ]
               [ td [ Class "hide-border" ] [ ]
                 td [ Class "hide-border" ] [ ]
                 td [ Class "hide-border"
                      ColSpan 5 ]
                    [ ul [ Class "expanded-release" ]
                          [ if fullRuntimeVersion r.Runtime then 
                                yield lif "Runtime version %O" r.Runtime.Value.Version
                            if fullSdkVersion r.Sdk then
                                yield lif "Sdk version %O" r.Sdk.Version
                            match r.Sdk.VsVersion with SomeText v -> yield lif "Included in Visual Studio %s" v | _ -> ()
                            match r.Sdk.CsharpLanguage with SomeText v -> yield lif "Supports C# %s" v | _ -> ()
                            match r.Sdk.FsharpLanguage with SomeText v -> yield lif "Supports F# %s" v | _ -> ()
                            match r.Sdk.VbLanguage with SomeText v -> yield lif "Supports Visual Basic %s" v | _ -> ()
                            match r.AspnetcoreRuntime with 
                            | Some a -> 
                                yield lif "ASP.NET Core Runtime %O" a.Version
                                match a.VersionAspnetcoremodule with 
                                | Some a when not a.IsEmpty -> 
                                    yield lif "ASP.NET Core IIS Module %O" a.Head
                                | _ -> ()
                            | None -> ()
                            match r.ReleaseNotes, state.ReleaseNotes with 
                            | None, _ -> ()
                            | Some _, Unloaded | Some _, Loading -> 
                                yield lif "Loading release notes"
                            | Some _, Loaded elem -> yield div [ Style [ MaxWidth 800; WhiteSpace "normal" ] ] [ elem ]
                            | Some _, Error ex -> 
                                yield lif "Loading release notes failed. Details: %s" ex.Message ] ] ]

        [ yield
              tr [ OnClick (fun _ -> (if state.Expanded then Collapse else Expand)
                                     |> dispatch) ]
                 [ td [ Class "hide-border" ] [ ]
                   td [ Class "expand-button" ] 
                      [ (if state.Expanded then View.chevronDown else View.chevronRight) [ ] ]
                   td [ ] [ str (string r.ReleaseVersion) ]
                   td [ ] [ View.dateToHtmlTime r.ReleaseDate ]
                   td [ ] [ str (match r.Runtime with
                                 | Some r -> Option.defaultValue (string r.Version) r.VersionDisplay
                                 | None -> "-") ]
                   td [ ] [ str (Option.defaultValue (string r.Sdk.Version) r.Sdk.VersionDisplay) ]
                   td [ ] ( securityIndicator ) ]
          if state.Expanded then yield expandedRelease () ]
