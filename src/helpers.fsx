module Helpers

#load "../.fake/build.fsx/intellisense.fsx"
#if !FAKE
    #r "Facades/netstandard" // Intellisense fix, see FAKE #1938
    #r "netstandard"
    #r "System.Net.Http"
#endif

open Fake.Core
open Fake.StaticGen
open Fake.StaticGen.Html.ViewEngine

open System
open System.Net.Http

open Markdig
open NetCore.Versions
open Nett
open Thoth.Json.Net

// Helpers
//////////
module Async =
    let map f x = async.Bind(x, f >> async.Return)

module Result =
    let rec allOk xs =
        match xs with
        | [] -> Ok []
        | Ok x :: rest -> Result.map (fun r -> x::r) (allOk rest)
        | Error x :: _ -> Error x

    let ofOption err = function
    | Some x -> Ok x
    | None -> Error err

module Option =
    type OptionBuilder() =
        member __.Bind(opt, f) = Option.bind f opt
        member __.Return(value) = Some value
        member __.ReturnFrom(value) = value
    
    let option = OptionBuilder()

module Version =
    let simplify version =
        match version.Preview with
        | Some prev -> { version with Preview = Some (prev.Split('-').[0]) }
        | None -> version

    let pad length version =
        if length > version.Numbers.Length then
            { version with Numbers = List.append version.Numbers (List.replicate (length - version.Numbers.Length) 0) }
        else
            version

module Decode =
    let version path value =
        match Decode.string path value with
        | Ok s ->
            match Version.parse s with
            | Some v -> Ok v
            | None -> (path, BadPrimitive("a version", value)) |> Error
        | Error v -> Error v

module Page =
    let map f p = { Url = p.Url; Content = f p.Content }
    let mapl f = List.map (map f)

module DateTime =
    let tryParse text =
        match DateTime.TryParse text with
        | (true, dt) -> Some dt
        | (false, _) -> None

let (|DateTime|_|) = DateTime.tryParse

// Site configuration
/////////////////////
type Config =
    { ReleasesIndexUrl : string
      Title : string
      Description : string

      MonoRepo : string
      MonoPath : string

      Year : int
      
      // Secrets, defined in secrets.toml
      GitHubClientId : string
      GitHubClientSecret : string }

let parseConfig config =
    let toml = Toml.ReadString(config)
    { ReleasesIndexUrl = toml.["releases-index-url"].Get()
      Title = toml.["title"].Get()
      Description = toml.["description"].Get()

      MonoRepo = toml.["mono-repo"].Get()
      MonoPath = toml.["mono-path"].Get()

      Year = let now = DateTime.UtcNow in now.Year
      
      GitHubClientId = toml.["gh-client-id"].Get()
      GitHubClientSecret = toml.["gh-client-secret"].Get() }

// Data dowloads
////////////////
let download (accept : string) (headers : (string * string) list) (url : string) =
    async {
        use http = new HttpClient()
        use req = new HttpRequestMessage(HttpMethod.Get, url)
        req.Headers.Add("Accept", accept)
        req.Headers.Add("User-Agent", "Fake.StaticGen")
        for (header, value) in headers do
            req.Headers.Add(header, value)
        Trace.tracefn "Downloading %s" url
        let! resp = http.SendAsync req |> Async.AwaitTask
        if resp.IsSuccessStatusCode then
            let! content = resp.Content.ReadAsStringAsync() |> Async.AwaitTask
            return Ok content
        else
            return Error (sprintf "Error fetching %s, response status %O" url resp.StatusCode)
    }

let getJsonH url headers = 
    download "application/json" headers url

let getJson url =
    getJsonH url []

let rewriteGithubUrls (url : string) =
    if url.StartsWith("https://github.com") then 
        url.Replace("https://github.com", "https://raw.githubusercontent.com")
           .Replace("/blob/", "/")
           .Replace("/tree/", "/")
           .Replace("/raw/", "/")
    else url

let downloadGh accept = rewriteGithubUrls >> download accept []
let getJsonGh = rewriteGithubUrls >> getJson

let base64 (str : string) =
    let bytes = System.Text.Encoding.UTF8.GetBytes(str)
    System.Convert.ToBase64String(bytes)

// Template
///////////
let mdPipeline (file : string) =
    MarkdownPipelineBuilder()
        .UsePipeTables()
        .UseAutoLinks()
        .UseAutoIdentifiers(Extensions.AutoIdentifiers.AutoIdentifierOptions.GitHub)
        .UseSyntaxHighlighting()
        .UseUrlRewriter(fun link -> 
            if Uri.IsWellFormedUriString(link.Url, UriKind.Absolute) || link.Url.StartsWith("#") then
                link.Url
            else if link.Url.StartsWith("/docs/about-mono/releases/") then
                link.Url.Replace("/docs/about-mono/releases", "/mono")
            else if file.Contains("/docs/about-mono/releases/") then
                "https://www.mono-project.com" + link.Url
            else
                file.Substring(0, file.LastIndexOf('/')) + "/" + link.Url)
        .Build()

let date () (d : DateTime) = let local = d.ToLocalTime() in local.ToString "yyyy-MM-dd"

let indicatorSymb symb text clas = 
    div [ _class "status-box" ] [ 
        span [ _class ("status-indicator " + clas)
               _title text ] 
             symb 
        span [] [ str text ] 
    ]