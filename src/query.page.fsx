module Query.Page

#load "../.fake/build.fsx/intellisense.fsx"

open Fake.StaticGen
open Fake.StaticGen.Html.ViewEngine

// Page template
////////////////
let titleText = "Query - "
let keywords = [ "query"; "search" ]
let description = "Search for releases of .NET Core, .NET Framework and Mono. "

let content =
    div [ _class "inner-container" ] [
        h1 [ _class "inner-spaced" ] [ str "Query" ]
        div [ _id "query-main" ] [
            str "Loading"
        ]
        XmlEngine.tag "script" [ _src "/_framework/blazor.webassembly.js" ] []
    ]