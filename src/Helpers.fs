namespace VersionsOfDotNet

open Fable.Helpers.React
open Fable.Helpers.React.Props
open System

type Loadable<'t> =
    | Unloaded
    | Loading
    | Error of exn
    | Loaded of 't

[<RequireQualifiedAccess>]
module Loadable =
    let isLoaded x =
        match x with
        | Loaded _ -> true
        | _ -> false

    let map f x =
        match x with
        | Unloaded -> Unloaded | Loading -> Loading | Error e -> Error e
        | Loaded t -> Loaded (f t)

    let unzip x =
        map fst x, map snd x

[<RequireQualifiedAccess>]
module Int =
    let parse i = 
        match Int32.TryParse i with
        | (true, r) -> Some r
        | _ -> None

[<RequireQualifiedAccess>]
module String =
    let split (sep: char) (s: string) = s.Split(sep) |> Array.toList
    let trim (s: string) = s.Trim()
    let toLowerInvariant (s: string) = s.ToLowerInvariant()
    let isEmpty (s: string) = String.IsNullOrEmpty(s)
    let isWhitespace (s: string) = String.IsNullOrWhiteSpace(s)

[<RequireQualifiedAccess>]
module Option =
    let mapList optionList = 
        if optionList |> List.forall Option.isSome
        then optionList |> List.map Option.get |> Some
        else None

module View =
    let chevronRight props = button props [ img [ Src "data:image/svg+xml;base64,PD94bWwgdmVyc2lvbj0iMS4wIiA/PjwhRE9DVFlQRSBzdmcgIFBVQkxJQyAnLS8vVzNDLy9EVEQgU1ZHIDEuMS8vRU4nICAnaHR0cDovL3d3dy53My5vcmcvR3JhcGhpY3MvU1ZHLzEuMS9EVEQvc3ZnMTEuZHRkJz48c3ZnIGhlaWdodD0iNTEycHgiIGlkPSJMYXllcl8xIiBzdHlsZT0iZW5hYmxlLWJhY2tncm91bmQ6bmV3IDAgMCA1MTIgNTEyOyIgdmVyc2lvbj0iMS4xIiB2aWV3Qm94PSIwIDAgNTEyIDUxMiIgd2lkdGg9IjUxMnB4IiB4bWw6c3BhY2U9InByZXNlcnZlIiB4bWxucz0iaHR0cDovL3d3dy53My5vcmcvMjAwMC9zdmciIHhtbG5zOnhsaW5rPSJodHRwOi8vd3d3LnczLm9yZy8xOTk5L3hsaW5rIj48cGF0aCBkPSJNMjk4LjMsMjU2TDI5OC4zLDI1NkwyOTguMywyNTZMMTMxLjEsODEuOWMtNC4yLTQuMy00LjEtMTEuNCwwLjItMTUuOGwyOS45LTMwLjZjNC4zLTQuNCwxMS4zLTQuNSwxNS41LTAuMmwyMDQuMiwyMTIuNyAgYzIuMiwyLjIsMy4yLDUuMiwzLDguMWMwLjEsMy0wLjksNS45LTMsOC4xTDE3Ni43LDQ3Ni44Yy00LjIsNC4zLTExLjIsNC4yLTE1LjUtMC4yTDEzMS4zLDQ0NmMtNC4zLTQuNC00LjQtMTEuNS0wLjItMTUuOCAgTDI5OC4zLDI1NnoiLz48L3N2Zz4=" ] ]
    let chevronDown  props = button props [ img [ Src "data:image/svg+xml;base64,PD94bWwgdmVyc2lvbj0iMS4wIiA/PjwhRE9DVFlQRSBzdmcgIFBVQkxJQyAnLS8vVzNDLy9EVEQgU1ZHIDEuMS8vRU4nICAnaHR0cDovL3d3dy53My5vcmcvR3JhcGhpY3MvU1ZHLzEuMS9EVEQvc3ZnMTEuZHRkJz48c3ZnIGhlaWdodD0iNTEycHgiIGlkPSJMYXllcl8xIiBzdHlsZT0iZW5hYmxlLWJhY2tncm91bmQ6bmV3IDAgMCA1MTIgNTEyOyIgdmVyc2lvbj0iMS4xIiB2aWV3Qm94PSIwIDAgNTEyIDUxMiIgd2lkdGg9IjUxMnB4IiB4bWw6c3BhY2U9InByZXNlcnZlIiB4bWxucz0iaHR0cDovL3d3dy53My5vcmcvMjAwMC9zdmciIHhtbG5zOnhsaW5rPSJodHRwOi8vd3d3LnczLm9yZy8xOTk5L3hsaW5rIj48cGF0aCBkPSJNMjU2LDI5OC4zTDI1NiwyOTguM0wyNTYsMjk4LjNsMTc0LjItMTY3LjJjNC4zLTQuMiwxMS40LTQuMSwxNS44LDAuMmwzMC42LDI5LjljNC40LDQuMyw0LjUsMTEuMywwLjIsMTUuNUwyNjQuMSwzODAuOSAgYy0yLjIsMi4yLTUuMiwzLjItOC4xLDNjLTMsMC4xLTUuOS0wLjktOC4xLTNMMzUuMiwxNzYuN2MtNC4zLTQuMi00LjItMTEuMiwwLjItMTUuNUw2NiwxMzEuM2M0LjQtNC4zLDExLjUtNC40LDE1LjgtMC4yTDI1NiwyOTguMyAgeiIvPjwvc3ZnPg==" ] ]

    let dateToHtmlTime (date: DateTime) = 
        let s = date.ToString("yyyy-MM-dd")
        time [ Props.DateTime s ] [ str s ]

    let errorView (ex: exn) retryFun =
        [ span [ Class "error-symbol" ] 
               [ str "×" ]
          span [ Class "error-title" ] 
               [ str "An error occurred." ]
          button [ Class "error-retry-button"
                   OnClick retryFun ] 
                 [ str "Try again" ]
          span [ Class "error-details" ]
               [ str (sprintf "Details: %s" ex.Message) ] ]
               