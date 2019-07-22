module Query

#load "./.fake/build.fsx/intellisense.fsx"
#if !FAKE
    #r "Facades/netstandard" // Intellisense fix, see FAKE #1938
    #r "netstandard"
#endif

#load "helpers.fsx"
open Helpers

#load "core.fsx"

open Fake.StaticGen

open System
open System.Text

open NetCore.Versions
open NetCore.Versions.Data
open Thoth.Json.Net

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

    let getSdks channels =
        [ for ch in channels do 
            for rel in ch.Releases do
                for sdk in Core.allSdks rel ->
                    { Version = sdk.Version
                      ReleaseLink = sprintf "%O/%O" ch.ChannelVersion rel.ReleaseVersion
                      ReleaseDate = rel.ReleaseDate
                      RuntimeVersion = sdk.RuntimeVersion
                      VsVersion = sdk.VsVersion
                      CsharpVersion = sdk.CsharpVersion
                      FsharpVersion = sdk.FsharpVersion
                      VbVersion = sdk.VbVersion } ]
        |> List.sortByDescending (fun sdk -> sdk.ReleaseDate)

    let getRuntimes channels =
        [ for ch in channels do
            for rel in ch.Releases do
                match rel.Runtime with
                | None -> ()
                | Some rt -> 
                    yield { Version = rt.Version
                            ReleaseLink = sprintf "%O/%O" ch.ChannelVersion rel.ReleaseVersion
                            ReleaseDate = rel.ReleaseDate
                            VsVersion = rt.VsVersion } ]
        |> List.sortByDescending (fun rt -> rt.ReleaseDate)

    let getReleases channels =
        channels
        |> List.map (fun ch -> ch.Releases)
        |> List.concat
        |> List.map (fun rel ->
            { Version = rel.ReleaseVersion
              ReleaseDate = rel.ReleaseDate
              Runtime = rel.Runtime |> Option.map (fun rt -> rt.Version)
              Sdks = Core.allSdks rel |> List.map (fun sdk -> sdk.Version)
              AspRuntime = rel.AspnetcoreRuntime |> Option.map (fun asp -> asp.Version)
              Cves = rel.CveList |> List.map (fun cve -> cve.CveId) })
        |> List.sortByDescending (fun rel -> rel.ReleaseDate)

// Query files
//////////////
let getDataFiles channels =
    let jsonFile path encoder objects =
        { Url = path
          Content = List.map encoder objects |> Encode.list |> Encode.toString 0 |> Encoding.UTF8.GetBytes }
    [ channels |> Core.getSdks |> jsonFile "/query/core/sdks.json" Core.Sdk.Encoder   
      channels |> Core.getRuntimes |> jsonFile "/query/core/runtimes.json" Core.Runtime.Encoder  
      channels |> Core.getReleases |> jsonFile "/query/core/releases.json" Core.Release.Encoder ]