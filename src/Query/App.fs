module Query.App

open Elmish
open Bolero
open Bolero.Html
open Data
open Language
open Evaluator
open System.Net.Http
open Microsoft.AspNetCore.Components
open System
open Thoth.Json.Net
open FsToolkit.ErrorHandling.AsyncResultCEExtensions

type Loadable<'t, 'e> = 
    | Unloaded 
    | Loading 
    | Loaded of 't 
    | Failed of 'e

type ParseResult = FParsec.CharParsers.ParserResult<Pipeline<PosPair>, unit>

type Model =
    { Cache : Map<string, seq<FieldMap>>
      Query : string
      AST : Loadable<Pipeline<PosPair>, Position * string>
      Data : Loadable<seq<FieldMap>, Position * string> }

type Message = 
    | UpdateQuery of string
    | ParseFinished of ParseResult
    | ParseFailed of Exception
    | EvalFinished of Result<seq<FieldMap>, Position * string>
    | EvalFailed of Exception
    | LoadDataFinished of Result<string * seq<FieldMap>, string>
    | LoadDataFailed of Exception

let parseCmd query = 
    Cmd.ofAsync (fun query -> async { return parse query }) query ParseFinished ParseFailed
let evalCmd data pipeline = 
    Cmd.ofAsync (fun pipeline -> async { return evalPipeline data pipeline }) pipeline EvalFinished EvalFailed
let loadDataCmd (http : HttpClient) decoder fieldmap dataSource = 
    Cmd.ofAsync (fun (dataSource : string) -> asyncResult {
        let url = sprintf "/query/%s.json" (dataSource.Split('.') |> String.concat "/")
        let! resp = http.GetAsync url |> Async.AwaitTask
        if resp.IsSuccessStatusCode then
            let! json = resp.Content.ReadAsStringAsync() |> Async.AwaitTask
            let! data = Decode.fromString (Decode.list decoder) json
            return dataSource, Seq.map fieldmap data
        else
            return! Error (sprintf "Error while downloading data, got response %O" resp.StatusCode)
    }) dataSource LoadDataFinished LoadDataFailed

let init _ = 
    { Cache = Map.empty 
      Query = "" 
      AST = Unloaded
      Data = Unloaded }, Cmd.none

let update http message model =
    match message with
    | UpdateQuery q -> 
        { model with Query = q; AST = Loading }, parseCmd q
    | ParseFinished (ParseResult.Success (pipeline, _, _)) -> 
        let dataSource = pipeline.DataSource.Value
        let cmd, data = 
            match model.Cache |> Map.tryFind dataSource with
            | Some data -> 
                evalCmd data pipeline, Loading
            | None ->
                let loadDataCmd decoder fieldmap = loadDataCmd http decoder fieldmap
                let loadDataCmd =
                    match dataSource with
                    | "core.releases" -> Ok (loadDataCmd Core.Release.Decoder Core.Release.FieldMap)
                    | "core.runtimes" -> Ok (loadDataCmd Core.Runtime.Decoder Core.Runtime.FieldMap)
                    | "core.sdks" -> Ok (loadDataCmd Core.Sdk.Decoder Core.Sdk.FieldMap)
                    | "framework.releases" -> Ok (loadDataCmd Framework.Release.Decoder Framework.Release.FieldMap)
                    | "mono.releases" -> Ok (loadDataCmd Mono.Release.Decoder Mono.Release.FieldMap)
                    | invalid -> errf (getPosition pipeline.DataSource) "Invalid data source '%s'" invalid
                
                match loadDataCmd with
                | Ok loadDataCmd -> loadDataCmd dataSource, Loading
                | Error err -> Cmd.none, Failed err

        { model with AST = Loaded pipeline; Data = data }, cmd
    | ParseFinished (ParseResult.Failure (mes, err, _)) ->
        { model with AST = Failed (Position.Single err.Position, mes) }, Cmd.none
    | ParseFailed ex ->
        let mes = sprintf "An exception occured: %s" ex.Message
        { model with AST = Failed (Position.None, mes) }, Cmd.none
    | EvalFinished (Ok data) -> 
        { model with Data = Loaded data }, Cmd.none
    | EvalFinished (Error (pos, mes)) ->
        { model with Data = Failed (pos, mes) }, Cmd.none
    | EvalFailed ex ->
        let mes = sprintf "An exception occured: %s" ex.Message
        { model with Data = Failed (Position.None, mes) }, Cmd.none
    | LoadDataFinished (Ok (dataSource, data)) ->
        let cmd =
            match model.AST with
            | Loaded ast when ast.DataSource.Value = dataSource ->
                evalCmd data ast
            | _ -> Cmd.none
        { model with Cache = model.Cache |> Map.add dataSource data; Data = Loading }, cmd
    | LoadDataFinished (Error mes) ->
        match model.Data with
        | Loading -> { model with Data = Failed (Position.None, mes) }, Cmd.none
        | _ -> model, Cmd.none
    | LoadDataFailed ex ->
        match model.Data with
        | Loading -> { model with Data = Failed (Position.None, ex.Message) }, Cmd.none
        | _ -> model, Cmd.none

let view model dispatch = 
    div [] [
        yield textarea 
            [ on.change (fun args -> dispatch (UpdateQuery (unbox args.Value))) ] 
            [ text model.Query ]
        match model.Data with
        | Unloaded -> ()
        | Loading -> 
            yield p [] [ text "Loading result" ]
        | Loaded rows when rows |> Seq.isEmpty |> not ->
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
        | Loaded _ -> 
            yield p [] [ text "No results" ]
        | Failed (pos, mes) ->
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
        Program.mkProgram init (update this.HttpClient) view
        #if DEBUG
        |> Program.withTrace (fun msg _ -> printfn "Message: %s" ((string msg).Replace("\n", " ")))
        #endif
        |> Program.withErrorHandler (fun (msg, exn) -> printfn "Unhandled error: %s: %A" msg exn)
