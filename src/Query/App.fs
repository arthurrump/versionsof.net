module Query.App

open Elmish
open Bolero
open Bolero.Html

type Model =
    { Num : int }

let initModel = { Num = 0 }

type Message = Increase | Decrease

let update message model =
    match message with
    | Increase -> { model with Num = model.Num + 1 }
    | Decrease -> { model with Num = model.Num - 1 }

let view model dispatch = 
    div [] [
        button [ on.click (fun _ -> dispatch Increase ) ] [ text "+" ]
        span [] [ textf "Count: %i" model.Num ]
        button [ on.click (fun _ -> dispatch Decrease ) ] [ text "-" ]
    ]

type App() =
    inherit ProgramComponent<Model, Message>()
    override this.Program = Program.mkSimple (fun _ -> initModel) update view
