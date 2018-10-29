namespace VersionsOfDotNet

open System
open Fable.Helpers.React
open Fable.Helpers.React.Props
open Version

module Search =
    type SearchSuggestion =
        { Text: string
          Label: string
          Valid: bool }

    type Model =
        { InFocus: bool
          Query: string
          Suggestions: SearchSuggestion list }

    type Msg =
        | FocusChanged of bool
        | QueryChanged of string

    type QueryPrefix =
        | Release
        | Runtime
        | Sdk
        | AspRuntime
        | AspModule

        static member all =
            [ Release; Runtime; Sdk; AspRuntime; AspModule ]

    let prefixToLabel qp =
        match qp with
        | Release -> ("rel", "Release")
        | Runtime -> ("rt", "Runtime")
        | Sdk -> ("sdk", "Sdk")
        | AspRuntime -> ("asp", "ASP.NET Core Runtime")
        | AspModule -> ("aspmod", "ASP.NET Core IIS Module")

    let (|PartialPrefix|Prefix|Empty|Unknown|) (query: string) =
        let q = query.Trim().ToLowerInvariant()
        let qa = q.Split() |> Array.toList
        let ts = String.concat " "
        match qa with
        | "r"::[] -> PartialPrefix [ Release; Runtime ]
        | "rt"::qs -> Prefix (Runtime, ts qs)
        | "re"::[] -> PartialPrefix [ Release ]
        | "rel"::qs -> Prefix (Release, ts qs)
        | "s"::[] | "sd"::[] -> PartialPrefix [ Sdk ]
        | "sdk"::qs -> Prefix (Sdk, ts qs)
        | "a"::[] | "as"::[] | "asp"::[] -> PartialPrefix [ AspRuntime; AspModule ]
        | "asp"::qs -> Prefix (AspRuntime, ts qs)
        | "aspm"::[] | "aspmo"::[] -> PartialPrefix [ AspModule ]
        | "aspmod"::qs -> Prefix (AspModule, ts qs)
        | [ "" ] -> Empty
        | _ -> Unknown q

    let sug text label valid = 
        { Text = text; Label = label; Valid = valid }

    let suggestionsForQuery query =
        match query with
        | PartialPrefix pfs -> 
            pfs 
            |> List.map (fun pf -> let text, label = prefixToLabel pf
                                   sug (sprintf "%s x.x.x" text) label false)
        | Prefix (pf, "") -> 
            let pretext, label = prefixToLabel pf
            [ sug (sprintf "%s x.x.x" pretext) label false ]
        | Prefix (pf, Version v) ->
            let _, label = prefixToLabel pf
            [ sug (string v) label true ]
        | Prefix (pf, text) ->
            let _, label = prefixToLabel pf
            [ sug text label false ]
        | Version v ->
            [ sug (string v) "" true ] @
            [ for pf in QueryPrefix.all -> 
                let text, label = prefixToLabel pf
                sug (sprintf "%s %O" text v) label true ]
        | Empty ->
            [ sug "x.x.x" "" false ] @
            [ for pf in QueryPrefix.all ->
                let text, label = prefixToLabel pf
                sug (sprintf "%s x.x.x" text) label false ]
        | text ->
            [ sug text "Invalid query" false ]

    let init () = 
        { InFocus = false
          Query = ""
          Suggestions = suggestionsForQuery "" }

    let update msg model = 
        match msg with
        | FocusChanged focus ->
            { model with InFocus = focus }
        | QueryChanged query ->
            { model with Query = query; Suggestions = suggestionsForQuery query }

    let sugLi sug =
        li [ TabIndex 0.0 ] 
           [ yield str sug.Text
             if not (String.IsNullOrWhiteSpace(sug.Label)) 
                then yield span [ Class "label" ] [ str sug.Label ] ]

    let view model dispatch =
        div [ Class "input-wrapper" 
              OnFocus (fun _ -> dispatch (FocusChanged true))
              OnBlur (fun _ -> dispatch (FocusChanged false)) ]
            [ yield input [ Placeholder "Find a version..."
                            Props.Type "search"
                            OnChange (fun e -> dispatch (QueryChanged e.Value))
                            Value model.Query
                            Class (if model.InFocus then "focus" else "") ]
              if model.InFocus then 
                yield div [ Id "search-suggestions" ]
                          [ ul [ ] (model.Suggestions |> List.map sugLi) ] ]