namespace VersionsOfDotNet

open Fable.PowerPack.Fetch
open Data
open Thoth.Json
open Fable.PowerPack.PromiseImpl

// TODO: Switch back to official repo

[<RequireQualifiedAccess>]
module Fetch =
    let [<Literal>] ReleasesIndexUrl = "https://raw.githubusercontent.com/arthurrump/dotnet-core/new-json-fixes/release-notes/releases-index.json"
                                     //"https://raw.githubusercontent.com/dotnet/core/master/release-notes/releases-index.json"

    let index () =
        promise {
            let! response = fetch ReleasesIndexUrl []
            let! json = response.text()
            let decoder = Decode.object (fun get -> get.Required.Field "releases-index" (Decode.list IndexEntry.Decoder))
            match Decode.fromString decoder json with
            | Ok res -> return res
            | Error e -> return failwith e
        }

    let channel (githubUrl: Url) =
        promise {
            let url = githubUrl.Replace("github.com/dotnet/core/blob/master", "raw.githubusercontent.com/arthurrump/dotnet-core/new-json-fixes")
                                      //"github.com/dotnet/core/blob", "raw.githubusercontent.com/dotnet/core"
            let! response = fetch url []
            let! json = response.text()
            match Decode.fromString Channel.Decoder json with
            | Ok res -> return githubUrl, res
            | Error e -> return failwith e
        }
