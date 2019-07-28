module Query.App

open Elmish
open Bolero
open Bolero.Html
open Language

type Model =
    { Query : string
      Eval : Result<seq<Map<string, obj>>, string> }

let initModel = 
    { Query = ""
      Eval = evaluateQuery "" }

type Message = 
    | UpdateQuery of string

let update message model =
    match message with
    | UpdateQuery q -> { model with Query = q; Eval = evaluateQuery q }

let view model dispatch = 
    div [] [
        yield textarea 
            [ on.change (fun args -> dispatch (UpdateQuery (unbox args.Value)))
              attr.style "font-family: Consolas;" ] 
            [ text model.Query ]
        match model.Eval with
        | Ok rows when rows |> Seq.isEmpty |> not ->
            yield table [ attr.style "font-family: Consolas;" ] [
                thead [] [ tr [] [
                    for kv in rows |> Seq.head ->
                        th [] [ text kv.Key ]
                ] ]
                tbody [] [
                    for row in rows -> tr [] [
                        for kv in row ->
                            td [] [ textf "%A" kv.Value ]
                    ]
                ]
            ]
        | Ok _ -> yield p [] [ text "No results" ]
        | Error mes ->
            yield pre 
                [ attr.style "font-family: Consolas; background-color: #fdd;" ] 
                [ textf "Error:\n%s" mes ]
    ]

type App() =
    inherit ProgramComponent<Model, Message>()
    override __.Program = 
        Program.mkSimple (fun _ -> initModel) update view
        #if DEBUG
        |> Program.withTrace (fun msg model -> printfn "Message: %s; Result: %s" ((sprintf "%A" msg).Replace("\n", " \\n ")) (match model.Eval with Ok _ -> "Success" | Error _ -> "Failed"))
        #endif
        |> Program.withErrorHandler (fun (msg, exn) -> printfn "Unhandled error: %s: %A" msg exn)
