module Query.App

open Elmish
open Bolero
open Bolero.Html
open Language
open System.Net.Http
open Microsoft.AspNetCore.Components

type Loadable<'t> = Loading | Loaded of 't

type Model =
    { Cache : Evaluation.DataCache
      Query : string
      Eval : Loadable<Result<seq<FieldMap>, Position * string>> }

type Message = 
    | UpdateQuery of string
    | LoadedResult of Result<seq<FieldMap>, Position * string>

let evalCmd dc query = Cmd.ofAsync (evaluateQuery dc) query LoadedResult (fun ex -> LoadedResult (Error (Position.None, ex.Message)))

let init dc = { Cache = dc; Query = ""; Eval = Loading }, evalCmd dc ""

let update message model =
    match message with
    | UpdateQuery q -> { model with Query = q; Eval = Loading }, evalCmd model.Cache q
    | LoadedResult res -> { model with Eval = Loaded res }, Cmd.none

let view model dispatch = 
    div [] [
        yield textarea 
            [ on.change (fun args -> dispatch (UpdateQuery (unbox args.Value))) ] 
            [ text model.Query ]
        match model.Eval with
        | Loading -> 
            yield p [] [ text "Loading result" ]
        | Loaded (Ok rows) when rows |> Seq.isEmpty |> not ->
            let rows = rows |> Seq.map (Map.toSeq >> Seq.sortBy (fun (_, (i, _)) -> i))
            yield table [] [
                thead [] [ tr [] [
                    for field, _ in rows |> Seq.head ->
                        th [] [ text field ]
                ] ]
                tbody [] [
                    for row in rows -> tr [] [
                        for _, (_, value) in row ->
                            td [] [ textf "%A" value ]
                    ]
                ]
            ]
        | Loaded (Ok _) -> 
            yield p [] [ text "No results" ]
        | Loaded (Error (pos, mes)) ->
            let pos =
                match pos with
                | Position.None -> ""
                | Position.Single p -> sprintf " at %i:%i" p.Line p.Column
                | Position.Range (s, e) -> sprintf " at %i:%i-%i:%i" s.Line s.Column e.Line e.Column
            yield pre 
                [ attr.classes [ "error" ] ] 
                [ textf "Error%s:\n%s" pos mes ]
    ]

type App() =
    inherit ProgramComponent<Model, Message>()

    [<Inject>]
    member val HttpClient = Unchecked.defaultof<HttpClient> with get, set

    override this.Program = 
        Program.mkProgram (fun _ -> init (Evaluation.DataCache(this.HttpClient))) update view
        #if DEBUG
        |> Program.withTrace (fun msg _ -> printfn "Message: %s" ((string msg).Replace("\n", " ")))
        #endif
        |> Program.withErrorHandler (fun (msg, exn) -> printfn "Unhandled error: %s: %A" msg exn)
