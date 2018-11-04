namespace VersionsOfDotNet

open System
open Fable.Helpers.React
open Fable.Helpers.React.Props
open Version
open Fable.Import.React
open Elmish.React

module Search =
    type SearchSuggestion =
        { Text: string
          Label: string
          Valid: bool }

    type Model =
        { InFocus: bool
          Query: string
          Suggestions: SearchSuggestion list
          SelectedSuggestion: SearchSuggestion
          Filter: string option }

    type Msg =
        | FocusChanged of bool
        | QueryChanged of string
        | SelectionChanged of SearchSuggestion
        | FilterSet of SearchSuggestion

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
            let text, label = prefixToLabel pf
            [ sug (sprintf "%s %O" text v) label true ]
        | Prefix (pf, text) ->
            let pftext, label = prefixToLabel pf
            [ sug (sprintf "%s %s" pftext text) (sprintf "%s - Invalid version" label) false ]
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
          Suggestions = suggestionsForQuery ""
          SelectedSuggestion = (suggestionsForQuery "").Head
          Filter = None }

    let update msg model = 
        match msg with
        | FocusChanged focus ->
            { model with InFocus = focus
                         SelectedSuggestion = if focus then model.SelectedSuggestion else model.Suggestions.Head }
        | QueryChanged query ->
            let sug = suggestionsForQuery query
            { model with Query = query; Suggestions = sug; SelectedSuggestion = sug.Head }
        | SelectionChanged selected ->
            { model with SelectedSuggestion = selected }
        | FilterSet sug ->
            { model with Filter = Some sug.Text; Query = sug.Text; InFocus = false }

    let ClassL l = Class (l |> String.concat " ")

    let selectedIndex model = 
        model.Suggestions |> List.findIndex (fun sug -> sug = model.SelectedSuggestion)

    let handleSearchKeyDown model dispatch (event: KeyboardEvent) =
        match event.key with
        | "ArrowDown" | "Down" ->
            let index = selectedIndex model
            let newIndex = min (index + 1) ((model.Suggestions |> List.length) - 1)
            let newSelected = model.Suggestions |> List.item newIndex
            dispatch (SelectionChanged newSelected)
            event.preventDefault()
        | "ArrowUp" | "Up" ->
            let index = selectedIndex model
            let newIndex = max (index - 1) 0
            let newSelected = model.Suggestions |> List.item newIndex
            dispatch (SelectionChanged newSelected)
            event.preventDefault()
        | "Escape" ->
            dispatch (FocusChanged false)
        | "Enter" ->
            if model.SelectedSuggestion.Valid 
            then dispatch (FilterSet model.SelectedSuggestion)
        | _ -> ()

    let sugLi dispatch selected sug =
        li [ ClassL [ if not sug.Valid then yield "invalid"
                      if sug = selected then yield "selected" ]
             TabIndex -1.0 // Focusable, but not in tab sequence
             OnMouseEnter (fun _ -> dispatch (SelectionChanged sug))
             Role "option"
             HTMLAttr.Custom ("aria-selected", (sug = selected))
             OnClick (fun _ -> if sug.Valid then dispatch (FilterSet sug)) ] 
           [ yield str sug.Text
             if not (String.IsNullOrWhiteSpace(sug.Label)) 
                then yield span [ Class "label" ] [ str sug.Label ] ]

    let view model dispatch =
        div [ Class "input-wrapper" 
              OnFocus (fun _ -> dispatch (FocusChanged true))
              OnBlur (fun _ -> dispatch (FocusChanged false))
              OnKeyDown (handleSearchKeyDown model dispatch) ]
            [ yield input [ Placeholder "Find a version..."
                            Props.Type "search"
                            OnChange (fun e -> dispatch (QueryChanged e.Value))
                            Helpers.valueOrDefault model.Query
                            SpellCheck false
                            AutoCorrect "off"
                            AutoComplete "off"
                            AutoCapitalize "off"
                            ClassL [ if model.InFocus then yield "focus" ] ]
              if model.InFocus then 
                yield div [ Id "search-suggestions" ]
                          [ ul [ Role "listbox" ] 
                               (model.Suggestions |> List.map (sugLi dispatch model.SelectedSuggestion)) ] ]