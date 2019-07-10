module Helpers

[<AutoOpen>]
module internal AsyncResult =
    type AsyncResultBuilder() =
        member __.Bind (x, f) = 
            async {
                match! x with
                | Ok x -> return! f x
                | Error x -> return Error x
            }

        member __.Return x = Ok x |> async.Return
        member __.ReturnFrom x = x
        member this.Zero () = this.Return ()

    let asyncResult = AsyncResultBuilder()

module internal Result =
    let rec allOk xs =
        match xs with
        | [] -> Ok []
        | Ok x :: rest -> Result.map (fun r -> x::r) (allOk rest)
        | Error x :: _ -> Error x

module internal Async =
    let map f x = async.Bind(x, f >> async.Return)