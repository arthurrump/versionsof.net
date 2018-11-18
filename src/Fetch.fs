namespace VersionsOfDotNet

open Fable.PowerPack.Fetch
open Data
open Thoth.Json
open Fable.PowerPack.PromiseImpl

// TODO: Switch back to official repo

[<RequireQualifiedAccess>]
module Fetch =
    let [<Literal>] ReleasesIndexUrl = "https://raw.githubusercontent.com/arthurrump/dotnet-core/new-json-fixes-master-pulled/release-notes/releases-index.json"
                                       //"https://raw.githubusercontent.com/dotnet/core/master/release-notes/releases-index.json"

    let private rawGithubUrl (githubUrl: Url) = 
        githubUrl.Replace("github.com/dotnet/core/blob/master", "raw.githubusercontent.com/arthurrump/dotnet-core/new-json-fixes-master-pulled")
                         //("github.com/dotnet/core/blob", "raw.githubusercontent.com/dotnet/core")

    let index () =
        promise {
            let! response = fetch ReleasesIndexUrl []
            let! json = response.text()
            let decoder = Decode.object (fun get -> get.Required.Field "releases-index" (Decode.list IndexEntry.Decoder))
            match Decode.fromString decoder json with
            | Ok res -> return res
            | Result.Error e -> return failwith e
        }

    let channel (githubUrl: Url) =
        promise {
            let url = rawGithubUrl githubUrl
            let! response = fetch url []
            let! json = response.text()
            match Decode.fromString Channel.Decoder json with
            | Ok res -> return res
            | Result.Error e -> return failwith e
        }

    let releaseNotes (githubUrl: Url) =
        promise {
            let url = rawGithubUrl githubUrl
            let! response = fetch url []
            return! response.text()
        }