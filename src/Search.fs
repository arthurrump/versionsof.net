namespace VersionsOfDotNet

open System
open Fable.Helpers.React
open Fable.Helpers.React.Props

module Search =
    type SearchSuggestion =
        { Text: string
          Label: string
          Valid: bool }

    type Model =
        { SearchFocus: bool
          SearchQuery: string
          Suggestions: SearchSuggestion list }

    type Msg =
        | SearchFocusChanged of bool
        | SearchQueryChanged of string

    let init () = 
        { SearchFocus = false
          SearchQuery = ""
          Suggestions = [ ] }

    let searchSuggestions m =
        let (|Release|Runtime|Sdk|AspRuntime|AspModule|Empty|Unknown|) (query: string) =
            let q = query.Trim().ToLowerInvariant()
            let qa = q.Split() |> Array.toList
            printfn "%A" qa
            match qa with
            | "rel"::qs | "release"::qs -> Release (String.concat " " qs)
            | "rt"::qs | "run"::qs | "runtime"::qs -> Runtime (String.concat " " qs)
            | "sdk"::qs -> Sdk (String.concat " " qs)
            | "asp"::qs | "aspnet"::qs | "asprt"::qs |
              "aspruntime"::qs | "aspnetrt"::qs -> AspRuntime (String .concat " " qs)
            | "aspmod"::qs | "iismod"::qs -> AspModule (String.concat " " qs)
            | [ "" ] -> printfn "Empty!"; Empty
            | _ -> printfn "Unknown!"; Unknown q

        let (|Version|_|) (input: string) =
            let i = input.Trim().ToLowerInvariant()
            let versionParts =
                i.Split('.') 
                |> Array.toList
                |> List.filter (fun i -> i <> "")
            let isVersionLastPart (last: string) =
                let ls = last.Split('-') |> Array.toList
                Int32.TryParse(ls.Head) |> fst
            let isVersion = not (List.isEmpty versionParts) && 
                            List.forall (fun i -> Int32.TryParse(i) |> fst || 
                                                  if i = List.last versionParts 
                                                  then isVersionLastPart i 
                                                  else false) versionParts
            if isVersion then Some i else None
            
        let (|StrLength|_|) length (input: string) =
            if input.Length = length then Some input else None

        let searchHints = 
            [ ("rel", "Release")
              ("rt", "Runtime")
              ("sdk", "Sdk")
              ("asp", "ASP.NET Core Runtime")
              ("aspmod", "ASP.NET Core IIS Module") ]

        let sugLi text label =
            li [ TabIndex 0.0 ] 
               [ yield str text
                 if not (String.IsNullOrWhiteSpace(label)) 
                    then yield span [ Class "label" ] [ str label ] ]

        let hintList version hints =
            hints |> List.map (fun (t, n) -> sugLi (sprintf "%s %s" t version) n)

        div [ Id "search-suggestions" ]
            [ ul [ ] 
                 ( match m.SearchQuery with
                   | Empty -> sugLi "*" "" :: sugLi "x.x.x" "" :: (searchHints |> hintList "x.x.x")
                   | Version x -> sugLi x "" :: (searchHints |> hintList x)
                   | "*" -> [ sugLi "*" "" ]
                   | StrLength 1 c
                   | StrLength 2 c -> searchHints |> List.filter (fun (s, _) -> s.StartsWith(c)) |> hintList "x.x.x"
                   | Release (Version x) -> [ sugLi x "Release" ]
                   | Release "" -> [ sugLi "x.x.x" "Release" ]
                   | Runtime (Version x) -> [ sugLi x "Runtime" ]
                   | Runtime "" -> [ sugLi "x.x.x" "Runtime" ]
                   | Sdk (Version x) -> [ sugLi x "Sdk" ]
                   | Sdk "" -> [ sugLi "x.x.x" "Sdk"]
                   | StrLength 3 c -> searchHints |> List.filter (fun (s, _) -> s.StartsWith(c)) |> hintList "x.x.x"
                   | AspRuntime (Version x) -> [ sugLi x "ASP.NET Core Runtime" ]
                   | AspRuntime "" -> [ sugLi "x.x.x" "ASP.NET Core Runtime" ]
                   | StrLength 4 c 
                   | StrLength 5 c -> searchHints |> List.filter (fun (s, _) -> s.StartsWith(c)) |> hintList "x.x.x"
                   | AspModule (Version x) -> [ sugLi x "ASP.NET Core IIS Module" ]
                   | AspModule "" -> [ sugLi "x.x.x" "ASP.NET Core IIS Module" ]
                   | x -> [ sugLi x "Invalid version" ] ) ]

    let update msg model = 
        match msg with
        | SearchFocusChanged focus ->
            model |> fun m -> { m with SearchFocus = focus }
        | SearchQueryChanged query ->
            model |> fun m -> { m with SearchQuery = query }

    let view model dispatch =
        div [ Class "input-wrapper" 
              OnFocus (fun _ -> dispatch (SearchFocusChanged true))
              OnBlur (fun _ -> dispatch (SearchFocusChanged false)) ]
            [ yield input [ Placeholder "Find a version..."
                            Props.Type "search"
                            OnChange (fun e -> dispatch (SearchQueryChanged e.Value))
                            Value model.SearchQuery
                            Class (if model.SearchFocus then "focus" else "") ]
              if model.SearchFocus then yield searchSuggestions model ]