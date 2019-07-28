module Query.Data

open System
open NetCore.Versions
open Thoth.Json.Net

// Helpers
//////////
module private Decode =
    let version path value =
        match Decode.string path value with
        | Ok s ->
            match Version.parse s with
            | Some v -> Ok v
            | None -> (path, BadPrimitive("a version", value)) |> Error
        | Error v -> Error v

// .NET Core
////////////
module Core =
    type Sdk =
        { Version : Version
          ReleaseLink : string // channel/release
          ReleaseDate : DateTime
          RuntimeVersion : Version option
          VsVersion : Version option
          CsharpVersion : Version option
          FsharpVersion : Version option
          VbVersion : Version option }

        static member Encoder sdk =
            Encode.object [
                yield "version", Encode.string (string sdk.Version)
                yield "release", Encode.string sdk.ReleaseLink
                yield "date", Encode.datetime sdk.ReleaseDate
                match sdk.RuntimeVersion with Some rt -> yield "runtime", Encode.string (string rt) | _ -> ()
                match sdk.VsVersion with Some rt -> yield "vs", Encode.string (string rt) | _ -> ()
                match sdk.CsharpVersion with Some rt -> yield "csharp", Encode.string (string rt) | _ -> ()
                match sdk.FsharpVersion with Some rt -> yield "fsharp", Encode.string (string rt) | _ -> ()
                match sdk.VbVersion with Some rt -> yield "vb", Encode.string (string rt) | _ -> ()
            ]

        static member Decoder =
            Decode.object (fun get ->
                { Version = get.Required.Field "version" Decode.version
                  ReleaseLink = get.Required.Field "release" Decode.string
                  ReleaseDate = get.Required.Field "date" Decode.datetime
                  RuntimeVersion = get.Optional.Field "runtime" Decode.version
                  VsVersion = get.Optional.Field "vs" Decode.version
                  CsharpVersion = get.Optional.Field "csharp" Decode.version
                  FsharpVersion = get.Optional.Field "fsharp" Decode.version
                  VbVersion = get.Optional.Field "vb" Decode.version })

    type Runtime =
        { Version : Version
          ReleaseLink : string // channel/release
          ReleaseDate : DateTime
          VsVersion : Version list }

        static member Encoder rt =
            Encode.object [
                "version", Encode.string (string rt.Version)
                "release", Encode.string rt.ReleaseLink
                "date", Encode.datetime rt.ReleaseDate
                "vs", Encode.list (rt.VsVersion |> List.map (string >> Encode.string))
            ]

        static member Decoder =
            Decode.object (fun get ->
                { Version = get.Required.Field "version" Decode.version
                  ReleaseLink = get.Required.Field "release" Decode.string
                  ReleaseDate = get.Required.Field "date" Decode.datetime
                  VsVersion = get.Required.Field "vs" (Decode.list Decode.version) })

    type Release =
        { Version : Version
          ReleaseDate : DateTime
          Runtime : Version option
          Sdks : Version list
          AspRuntime : Version option
          Cves : string list }

        static member Encoder rel =
            Encode.object [
                yield "version", Encode.string (string rel.Version)
                yield "date", Encode.datetime rel.ReleaseDate
                match rel.Runtime with Some rt -> yield "runtime", Encode.string (string rt) | _ -> ()
                yield "sdks", Encode.list (rel.Sdks |> List.map (string >> Encode.string))
                match rel.AspRuntime with Some asp -> yield "asp", Encode.string (string asp) | _ -> ()
                yield "cves", Encode.list (rel.Cves |> List.map Encode.string)
            ]

        static member Decoder =
            Decode.object (fun get ->
                { Version = get.Required.Field "version" Decode.version
                  ReleaseDate = get.Required.Field "date" Decode.datetime
                  Runtime = get.Optional.Field "runtime" Decode.version
                  Sdks = get.Required.Field "sdks" (Decode.list Decode.version)
                  AspRuntime = get.Optional.Field "asp" Decode.version
                  Cves = get.Required.Field "cves" (Decode.list Decode.string) })

    let releases = [
        { Version = { Numbers = [1;0;0]; Preview = None }
          ReleaseDate = DateTime(2018, 12, 1)
          Runtime = Some { Numbers = [1;0;0]; Preview = None }
          Sdks = [ { Numbers = [1;0;0]; Preview = None } ]
          AspRuntime = None
          Cves = [ "CVE-2019-123"; "CVE-2019-124" ] }
        { Version = { Numbers = [1;1;0]; Preview = None }
          ReleaseDate = DateTime(2019, 1, 1)
          Runtime = Some { Numbers = [1;1;0]; Preview = None }
          Sdks = [ { Numbers = [1;1;0]; Preview = None } ]
          AspRuntime = Some { Numbers = [1;1;0]; Preview = None }
          Cves = [] }
        { Version = { Numbers = [1;2;0]; Preview = None }
          ReleaseDate = DateTime(2019, 2, 5)
          Runtime = None
          Sdks = [ { Numbers = [1;2;0]; Preview = None }
                   { Numbers = [1;2;1]; Preview = None } ]
          AspRuntime = None
          Cves = [ "CVE-2019-126"; "CVE-2019-128" ] }
        { Version = { Numbers = [2;0;0]; Preview = Some "preview" }
          ReleaseDate = DateTime(2019, 4, 7)
          Runtime = Some { Numbers = [2;0;0]; Preview = Some "preview" }
          Sdks = [ { Numbers = [2;0;0]; Preview = Some "preview" }
                   { Numbers = [2;0;0]; Preview = Some "preview-2" } ]
          AspRuntime = None
          Cves = [ "CVE-2019-123" ] }
    ]

// .NET Framework
/////////////////
module Framework =
    type Release =
        { Version : Version
          ReleaseDate : DateTime
          ClrVersion : Version
          IncludedInWindows : string option
          IncludedInServer : string option
          InstallableOnWindows : string list
          InstallableOnServer : string list }

        static member Encoder rel =
            Encode.object [
                yield "version", Encode.string (string rel.Version)
                yield "date", Encode.datetime rel.ReleaseDate
                yield "clr", Encode.string (string rel.ClrVersion)
                match rel.IncludedInWindows with Some win -> yield "windows", Encode.string win | _ -> ()
                match rel.IncludedInServer with Some ser -> yield "server", Encode.string ser | _ -> ()
                yield "windows-inst", Encode.list (rel.InstallableOnWindows |> List.map Encode.string)
                yield "server-inst", Encode.list (rel.InstallableOnServer |> List.map Encode.string)
            ]

        static member Decoder =
            Decode.object (fun get ->
                { Version = get.Required.Field "version" Decode.version
                  ReleaseDate = get.Required.Field "date" Decode.datetime
                  ClrVersion = get.Required.Field "clr" Decode.version
                  IncludedInWindows = get.Optional.Field "windows" Decode.string
                  IncludedInServer = get.Optional.Field "server" Decode.string
                  InstallableOnWindows = get.Required.Field "windows-inst" (Decode.list Decode.string)
                  InstallableOnServer = get.Required.Field "server-inst" (Decode.list Decode.string) })

// Mono
///////
module Mono =
    type Release =
        { Version : Version
          ReleaseDate : DateTime option
          Skipped : bool }

        static member Encoder rel = 
            Encode.object [
                yield "version", Encode.string (string rel.Version)
                match rel.ReleaseDate with Some d -> yield "date", Encode.datetime d | _ -> ()
                if rel.Skipped then yield "skipped", Encode.bool rel.Skipped
            ]

        static member Decoder =
            Decode.object (fun get ->
                { Version = get.Required.Field "version" Decode.version
                  ReleaseDate = get.Optional.Field "date" Decode.datetime
                  Skipped = get.Optional.Field "skipped" Decode.bool |> Option.defaultValue false })
