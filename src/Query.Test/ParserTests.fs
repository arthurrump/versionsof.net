module Query.Test.ParserTests

open Expecto
open FsCheck
open System

open Query.Language

type QueryGen() =
    static let isIdentifier str = 
        str |> String.IsNullOrEmpty |> not
        && str.StartsWith('.') |> not
        && str |> String.forall (fun c -> (c > 'a' && c < 'z') || (c > 'A' && c < 'Z') || c = '.')
    
    static member Expression() =
        let rec expr' = function
            | 0 -> Gen.oneof [
                    Arb.generate |> Gen.where isIdentifier |> Gen.map Field
                    Arb.generate |> Gen.map Literal ]
            | x -> Gen.oneof [
                    Gen.zip3 (expr' (x/2)) Arb.generate (expr' (x/2)) |> Gen.map Comparison
                    Gen.zip3 (expr' (x/2)) Arb.generate (expr' (x/2)) |> Gen.map BooleanExpression
                    expr' (x/2) |> Gen.map Negation ] 
        Gen.sized expr' |> Arb.fromGen

    static member Operation() =
        Gen.oneof [
            Arb.generate |> Gen.map Where
            Gen.nonEmptyListOf Arb.generate |> Gen.map Select
        ] |> Arb.fromGen

    static member Pipeline() =
        gen { 
            let! ds = Arb.generate |> Gen.where isIdentifier
            let! ops = Arb.generate
            return { DataSource = ds; Operations = ops }
        } |> Arb.fromGen

let config = 
    { FsCheckConfig.defaultConfig with 
        arbitrary = [ typeof<QueryGen> ] }

let testProp name = testPropertyWithConfig config name

[<Tests>]
let tests = 
    testList "Parser" [
        testPropertyWithConfig config "Parse -> Pretty -> Parse" <| fun pipeline ->
            let pretty = PrettyPrint.prettyPipeline Cs pipeline
            match parse pretty with
            | FParsec.CharParsers.Success (result, _, _) ->
                Expect.equal result pipeline "Parsed equals input"
            | FParsec.CharParsers.Failure (mes, _, _) ->
                failtestf "Parsing failed: %s" mes
    ]
