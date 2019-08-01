module Query.Test.ParserTests

open Expecto
open FsCheck
open System

open NetCore.Versions
open Query.Language

let annotate value = { Value = value; Annotation = () } 
let clearAnnotations pipeline =
    let clear anno = annotate anno.Value
    let rec clearExpr expr =
        match expr.Value with
        | Comparison (l, op, r) -> Comparison (clearExpr l, op, clearExpr r)
        | BooleanExpression (l, op, r) -> BooleanExpression (clearExpr l, op, clearExpr r)
        | Negation e -> Negation (clearExpr e)
        | Field f -> Field f
        | Literal l -> Literal l
        |> annotate
    let rec clearOp op =
        match op.Value with
        | Where expr -> Where (clearExpr expr)
        | Select fs -> Select (fs |> List.map clear)
        | SortBy (b, expr) -> SortBy (b, clearExpr expr)
        |> annotate
    { DataSource = clear pipeline.DataSource
      Operations = List.map clearOp pipeline.Operations }

type QueryGen() =
    static let isIdentifier str = 
        str |> String.IsNullOrEmpty |> not
        && str.StartsWith('.') |> not
        && str |> String.forall (fun c -> (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || c = '.')

    static let isVersionPreview str =
        str |> String.IsNullOrEmpty |> not
        && str |> String.forall (fun c -> (c >= 'a' && c <= 'z') 
                                          || (c >= 'A' && c <= 'Z') 
                                          || c = '.' || c = '_' || c = '-')
    
    static member DateTime() =
        gen {
            let! year = Gen.choose (1990, 2060)
            let! month = Gen.choose (1, 12)
            let! day = Gen.choose (1, DateTime.DaysInMonth (year, month))
            return DateTime(year, month, day)
        } |> Arb.fromGen

    static member Version() =
        gen {
            let! ns = Gen.nonEmptyListOf Arb.generate
            let! pre = Arb.generate |> Gen.where isVersionPreview |> Gen.optionOf
            return { Numbers = ns; Preview = pre }
        } |> Arb.fromGen

    static member String() =
        Arb.Default.String() 
        |> Arb.filter (fun s ->
            s <> null && s |> String.forall (fun c -> c <> '\n' && c <> '\r' && c <> '\t' && c <> '\v'))

    static member Expression() =
        { new Arbitrary<_>() with 
            override __.Generator = 
                let rec gen' = function
                | 0 -> Gen.oneof [
                        Arb.generate |> Gen.where isIdentifier |> Gen.map Field
                        Arb.generate |> Gen.map Literal ]
                       |> Gen.map annotate
                | x -> Gen.oneof [
                        Gen.zip3 (gen' (x/2)) Arb.generate (gen' (x/2)) |> Gen.map Comparison
                        Gen.zip3 (gen' (x/2)) Arb.generate (gen' (x/2)) |> Gen.map BooleanExpression
                        gen' (x/2) |> Gen.map Negation ]
                       |> Gen.map annotate
                Gen.sized gen'
            override __.Shrinker expr = 
                match expr.Value with
                | Field name -> Arb.shrink name |> Seq.map (Field >> annotate)
                | Literal lit -> Arb.shrink lit |> Seq.map (Literal >> annotate)
                | Comparison (e1, _, e2) -> seq [ e1; e2 ]
                | BooleanExpression (e1, _, e2) -> seq [ e1; e2 ]
                | Negation e -> seq [ e ] }

    static member Operation() =
        Gen.oneof [
            Arb.generate |> Gen.map Where
            Gen.nonEmptyListOf (Arb.generate |> Gen.where isIdentifier |> Gen.map annotate) |> Gen.map Select
            Arb.generate |> Gen.map SortBy
        ] |> Arb.fromGen

    static member Pipeline() =
        { new Arbitrary<_>() with
            override __.Generator =
                gen { 
                    let! ds = Arb.generate |> Gen.where isIdentifier
                    let! ops = Arb.generate
                    return { DataSource = annotate ds; Operations = ops }
                }
            override __.Shrinker p =
                Arb.shrink p.Operations
                |> Seq.map (fun ops -> { DataSource = p.DataSource; Operations = ops }) }

let config = 
    { FsCheckConfig.defaultConfig with 
        arbitrary = [ typeof<QueryGen> ] }

let testProp name = testPropertyWithConfig config name
let etestProp gen name = etestPropertyWithConfig gen config name

[<Tests>]
let tests = 
    testList "Parser" [
        testProp "Parse -> Pretty -> Parse" <| fun (style, pipeline) ->
            let pretty = PrettyPrint.prettyPipeline style pipeline
            match parse pretty with
            | FParsec.CharParsers.Success (result, _, _) ->
                Expect.equal (clearAnnotations result) pipeline "Parsed equals input"
            | FParsec.CharParsers.Failure (mes, _, _) ->
                failtestf "Parsing failed: %s" mes
    ]
