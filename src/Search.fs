namespace VersionsOfDotNet

open System
open VersionsOfDotNet
open Fable.Helpers.React
open Fable.Helpers.React.Props
open Version
open Fable.Import.React
open Fable.Import
open Elmish.React
open Elmish

module Search =
    type QueryPrefix =
        | Release
        | Runtime
        | Sdk
        | AspRuntime
        | AspModule

        static member all =
            [ Release; Runtime; Sdk; AspRuntime; AspModule ]

    type Filter =
        | ShowAll
        | Generic of Version
        | WithPrefix of QueryPrefix * Version

    type SuggestionValidity =
        | Valid of Filter
        | Invalid

    type SearchSuggestion =
        { Text: string
          Label: string
          Valid: SuggestionValidity }

    type State =
        { InFocus: bool
          Query: string
          Suggestions: SearchSuggestion list
          SelectedSuggestion: SearchSuggestion
          Filter: Filter }

    type Msg =
        | FocusChanged of bool
        | QueryChanged of string
        | SelectionChanged of SearchSuggestion
        | FilterSet of Filter * query: string

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

    let suggestionsForQuery filter query =
        match query with
        | PartialPrefix pfs -> 
            pfs 
            |> List.map (fun pf -> let text, label = prefixToLabel pf
                                   sug (sprintf "%s x.x.x" text) label Invalid)
        | Prefix (pf, "") -> 
            let pretext, label = prefixToLabel pf
            [ sug (sprintf "%s x.x.x" pretext) label Invalid ]
        | Prefix (pf, Version v) ->
            let text, label = prefixToLabel pf
            [ sug (sprintf "%s %O" text v) label (Valid (WithPrefix (pf, v))) ]
        | Prefix (pf, text) ->
            let pftext, label = prefixToLabel pf
            [ sug (sprintf "%s %s" pftext text) (sprintf "%s - Invalid version" label) Invalid ]
        | Version v ->
            [ sug (string v) "" (Valid (Generic v)) ] @
            [ for pf in QueryPrefix.all -> 
                let text, label = prefixToLabel pf
                sug (sprintf "%s %O" text v) label (Valid (WithPrefix (pf, v))) ]
        | Empty ->
            (match filter with ShowAll -> [ ] | _ -> [ sug "" "Remove filter" (Valid ShowAll) ]) @
            [ sug "x.x.x" "" Invalid ] @
            [ for pf in QueryPrefix.all ->
                let text, label = prefixToLabel pf
                sug (sprintf "%s x.x.x" text) label Invalid ]
        | text ->
            [ sug text "Invalid query" Invalid ]

    let init () = 
        { InFocus = false
          Query = ""
          Suggestions = suggestionsForQuery ShowAll ""
          SelectedSuggestion = (suggestionsForQuery ShowAll "").Head
          Filter = ShowAll }, Cmd.none

    let update msg model = 
        match msg with
        | FocusChanged focus ->
            { model with InFocus = focus
                         SelectedSuggestion = if focus then model.SelectedSuggestion else model.Suggestions.Head },
            Cmd.none
        | QueryChanged query ->
            let sug = suggestionsForQuery model.Filter query
            { model with Query = query; Suggestions = sug; SelectedSuggestion = sug.Head }, Cmd.none
        | SelectionChanged selected ->
            { model with SelectedSuggestion = selected }, Cmd.none
        | FilterSet (filter, queryText) ->
            let sugs = suggestionsForQuery filter queryText
            { model with Filter = filter; Query = queryText
                         Suggestions = sugs; SelectedSuggestion = sugs.Head }, Cmd.none

    module private Refs =
        let mutable unfocusDiv : Browser.HTMLElement = null

    let ClassL l = Class (l |> String.concat " ")

    let unfocus _ = if Refs.unfocusDiv <> null then Refs.unfocusDiv.focus ()

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
            unfocus ()
        | "Enter" ->
            match model.SelectedSuggestion.Valid with
            | Valid filter -> 
                unfocus ()
                dispatch (FilterSet (filter, model.SelectedSuggestion.Text))
            | Invalid -> ()
        | _ -> ()

    let sugLi dispatch selected sug =
        li [ ClassL [ if sug.Valid = Invalid then yield "invalid"
                      if sug = selected then yield "selected" ]
             TabIndex -1 // Focusable, but not in tab sequence
             OnMouseEnter ( match sug.Valid with 
                            | Valid _ -> fun _ -> dispatch (SelectionChanged sug)
                            | Invalid -> fun _ -> () )
             Role "option"
             HTMLAttr.Custom ("aria-selected", (sug = selected))
             OnClick (fun _ -> match sug.Valid with 
                               | Valid filter -> 
                                   unfocus ()
                                   dispatch (FilterSet (filter, sug.Text)) 
                               | Invalid -> ()) ] 
           [ yield str sug.Text
             if not (String.IsNullOrWhiteSpace(sug.Label)) 
                then yield span [ Class "label" ] [ str sug.Label ] ]

    let view model dispatch = 
        [ div [ Class "input-wrapper" 
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
                                 (model.Suggestions |> List.map (sugLi dispatch model.SelectedSuggestion)) ]
                if model.Filter <> ShowAll then 
                  yield a [ Role "button"
                            OnClick <| fun _ -> dispatch (FilterSet (ShowAll, ""))
                            Id "remove-filter-link" ] 
                          [ str "Remove filter"] ]
          div [ TabIndex -1 
                Props.Ref (fun elem -> Refs.unfocusDiv <- elem :?> Browser.HTMLElement) ] [ ] ]
