module Helpers

open Microsoft.FSharp.Core.Printf

module internal Async =
    let map f x = async.Bind(x, f >> async.Return)

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

        member __.Using (disposable : #System.IDisposable, body) =
            try body disposable finally if disposable <> null then disposable.Dispose()

    let asyncResult = AsyncResultBuilder()

    let awaitTask t = t |> Async.AwaitTask |> Async.map Ok
    let failf fmt = kprintf (Error >> async.Return) fmt

module internal Result =
    let rec allOk xs =
        match xs with
        | [] -> Ok []
        | Ok x :: rest -> Result.map (fun r -> x::r) (allOk rest)
        | Error x :: _ -> Error x
