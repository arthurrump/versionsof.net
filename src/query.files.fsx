module Query.Files

#load "../.fake/build.fsx/intellisense.fsx"
#if !FAKE
    #r "Facades/netstandard" // Intellisense fix, see FAKE #1938
    #r "netstandard"
#endif

#load "helpers.fsx"
open Helpers

#load "./Query/Data.fs"

#load "core.fsx"
#load "framework.fsx"
#load "mono.fsx"

open Fake.StaticGen

open System
open System.Text

open NetCore.Versions
open NetCore.Versions.Data
open Thoth.Json.Net

// .NET Core
////////////
module Core =
    open Query.Data.Core

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

    let getReleases =
        List.map (fun ch -> ch.Releases)
        >> List.concat
        >> List.map (fun rel ->
            { Version = rel.ReleaseVersion
              ReleaseDate = rel.ReleaseDate
              Runtime = rel.Runtime |> Option.map (fun rt -> rt.Version)
              Sdks = Core.allSdks rel |> List.map (fun sdk -> sdk.Version)
              AspRuntime = rel.AspnetcoreRuntime |> Option.map (fun asp -> asp.Version)
              Cves = rel.CveList |> List.map (fun cve -> cve.CveId) })
        >> List.sortByDescending (fun rel -> rel.ReleaseDate)

// .NET Framework
/////////////////
module Framework =
    open Query.Data.Framework

    let getReleases : Framework.Release list -> Release list =
        List.map (fun rel ->
            { Version = rel.Version
              ReleaseDate = rel.ReleaseDate
              ClrVersion = rel.ClrVersion
              IncludedInWindows = rel.IncludedInWindows
              IncludedInServer = rel.IncludedInServer
              InstallableOnWindows = rel.InstallableOnWindows
              InstallableOnServer = rel.InstallableOnServer })

// Mono
///////
module Mono =
    open Query.Data.Mono

    let getReleases : Mono.Release list -> Release list =
        List.map (fun rel ->
            let date, skipped =
                match rel.ReleaseDate with
                | Mono.Released date -> Some date, false
                | Mono.Skipped -> None, true
                | Mono.Stub -> None, false
            { Version = rel.Version
              ReleaseDate = date
              Skipped = skipped })

// Query files
//////////////
open Query.Data
let getDataFiles coreChannels frameworkReleases monoReleases =
    let jsonFile path encoder objects =
        { Url = path
          Content = List.map encoder objects |> Encode.list |> Encode.toString 0 |> Encoding.UTF8.GetBytes }
    [ coreChannels |> Core.getSdks |> jsonFile "/query/core/sdks.json" Core.Sdk.Encoder   
      coreChannels |> Core.getRuntimes |> jsonFile "/query/core/runtimes.json" Core.Runtime.Encoder  
      coreChannels |> Core.getReleases |> jsonFile "/query/core/releases.json" Core.Release.Encoder
      frameworkReleases |> Framework.getReleases |> jsonFile "/query/framework/releases.json" Framework.Release.Encoder
      monoReleases |> Mono.getReleases |> jsonFile "/query/mono/releases.json" Mono.Release.Encoder ]