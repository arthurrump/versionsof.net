#r "paket:
nuget Fake.Core.Target 
nuget Fake.DotNet.Cli
nuget Fake.DotNet.Testing.Expecto
nuget Fake.StaticGen
nuget Fake.StaticGen.Html
nuget Fake.StaticGen.Markdown
nuget MarkdigExtensions.SyntaxHighlighting
nuget MarkdigExtensions.UrlRewriter
nuget Nett
nuget YamlDotNet
nuget NetCoreVersions
nuget FSharp.Data
nuget FSharpPlus //"
#load "./.fake/build.fsx/intellisense.fsx"

open Fake.Core
open Fake.Core.TargetOperators
open Fake.IO
open Fake.IO.Globbing.Operators
open Fake.DotNet
open Fake.DotNet.Testing

open Markdig
open Nett

#load "src/site.fsx"

let (</>) = Path.combine

let [<Literal>] configFile = "config.toml"
let [<Literal>] secretsFile = "secrets.toml"

let query = __SOURCE_DIRECTORY__ </> "src" </> "Query"
let queryTest = __SOURCE_DIRECTORY__ </> "src" </> "Query.Test"

// Targets
//////////
Target.create "Query.Build" <| fun _ ->
    DotNet.build id query

Target.create "Query.Test" <| fun _ ->
    DotNet.build id queryTest
    Expecto.run id (!! (queryTest </> "bin" </> "Release" </> "**" </> "*.Test.dll"))

"Query.Build" ==> "Query.Test"

Target.create "Query.Publish" <| fun _ ->
    DotNet.publish id query
    Shell.copyDir 
        ("public" </> "query" </> "_framework") 
        (query </> "bin" </> "Release" </> "netstandard2.0" </> "publish" </> "Query" </> "dist" </> "_framework")
        (fun _ -> true)

"Query.Build" ==> "Query.Publish"

Target.create "Static.Generate" <| fun _ ->
    if File.exists secretsFile then
        let config = 
            [ configFile; secretsFile ]
            |> List.map File.readAsString
            |> String.concat "\n" 
            |> Helpers.parseConfig
        Site.generateSite config |> Async.RunSynchronously
    else
        Trace.traceErrorfn "Could not find file '%s'. Please create one using the Configure target." secretsFile

"Static.Generate" ?=> "Query.Publish"

Target.create "Build" <| fun _ -> ()
"Static.Generate" ==> "Build"
"Query.Publish" ==> "Build"

Target.create "Configure" <| fun p ->
    match p.Context.Arguments with
    | [ ghClientId; ghClientSecret ] ->
        let toml = Toml.Create()
        toml.Add("gh-client-id", ghClientId) |> ignore
        toml.Add("gh-client-secret", ghClientSecret) |> ignore
        Toml.WriteFile(toml, secretsFile)
    | _ ->
        Trace.traceErrorfn "Invalid arguments for Configure"
        Trace.traceErrorfn "Required arguments: [GitHub Client ID] [GitHub Client Secret]"
          
Target.runOrDefaultWithArguments "Build"