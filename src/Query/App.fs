module Query.App

open Elmish
open Bolero
open Bolero.Html
open FParsec
open Parser

type Model =
    { Query : string
      Parsed : ParserResult<Pipeline, unit> }

let initModel = 
    { Query = ""
      Parsed = parsePipeline "" }

type Message = 
    | UpdateQuery of string

let update message model =
    match message with
    | UpdateQuery q -> { model with Query = q; Parsed = parsePipeline q }

let view model dispatch = 
    div [] [
        textarea 
            [ on.change (fun args -> dispatch (UpdateQuery (unbox args.Value)))
              attr.style "font-family: Consolas;" ] 
            [ text model.Query ]
        pre [ attr.style "font-family: Consolas;" ] 
            [ textf "%A" model.Parsed ]
    ]

type App() =
    inherit ProgramComponent<Model, Message>()
    override this.Program = Program.mkSimple (fun _ -> initModel) update view
