module Query.Language

open System
open NetCore.Versions

type Pipeline = 
    { DataSource : string
      Operations : Operation list }

and Operation =
    | Where of Expression
    | Select of fields : string list

and Expression =
    | Comparison of Expression * CompOperator * Expression
    | BooleanExpression of Expression * BoolOperator * Expression
    | Negation of Expression
    | Field of string
    | Literal of Literal

and CompOperator = Equal | NotEqual | Less | LessEqual | Greater | GreaterEqual
and BoolOperator = And | Or

and Literal =
    | NoneLiteral
    | StringLiteral of string
    | VersionLiteral of Version
    | DateLiteral of DateTime

module private Parser =
    open FParsec

    let notSpaces = isNoneOf [ ' '; '\t'; '\r'; '\n' ]
    let ws p = spaces >>. p .>> spaces

    let pIdentifier = many1SatisfyL (fun c -> isAsciiLetter c || c = '.' || c = '-') "identifier"

    let pNoneLiteral = "'none'" |> choiceL [ pstringCI "none"; pstringCI "null" ] >>% NoneLiteral
    let pStringLiteral = between (pchar '"') (pchar '"') (manySatisfy ((<>) '"')) |>> StringLiteral <?> "a string"
    let pVersionLiteral = pipe3 pint32 (many (pchar '.' >>. pint32)) (opt (pchar '-' >>. manySatisfy notSpaces)) (fun n ns pre -> VersionLiteral { Numbers = n::ns; Preview = pre }) <?> "a version"
    let pDateLiteral = pipe3 pint32 (pchar '-' >>. pint32) (pchar '-' >>. pint32) (fun y m d -> DateLiteral (DateTime(y, m, d))) <?> "a date (yyyy-mm-dd)"
    let pLiteral = pNoneLiteral <|> pStringLiteral <|> pVersionLiteral <|> pDateLiteral

    let pCompOperator = 
        "comparison operator" |> choiceL 
            [ pstring "==" >>% Equal
              pstring "="  >>% Equal
              pstring "!=" >>% NotEqual
              pstring "<>" >>% NotEqual
              pstring "<=" >>% LessEqual
              pstring "<"  >>% Less
              pstring ">=" >>% GreaterEqual
              pstring ">"  >>% Greater ]
    
    let pBoolOperator =
        "boolean operator" |> choiceL
            [ pstringCI "and" .>> spaces1 >>% And
              pstring "&&"                >>% And
              pstringCI "or" .>> spaces1  >>% Or
              pstring "||"                >>% Or ]

    let pExpression, pExpressionImpl = createParserForwardedToRef()
    let pNegation = (pstring "!" <|> (pstringCI "not" .>> spaces1)) >>. pExpression |>> Negation
    let pBasicExpression =
        choice 
            [ pNegation
              (pLiteral |>> Literal)
              (pIdentifier |>> Field)
              between (pchar '(') (pchar ')') pExpression ]
        |> ws
    
    let pComparison = pipe3 pBasicExpression pCompOperator pExpression (fun ex1 op ex2 -> Comparison (ex1, op, ex2))
    let pBooleanExpression = pipe3 (attempt pComparison <|> pBasicExpression) pBoolOperator pExpression (fun ex1 op ex2 -> BooleanExpression (ex1, op, ex2))
    pExpressionImpl :=
        "expression" |> choiceL 
            [ attempt pComparison
              attempt pBooleanExpression
              pBasicExpression ]
        |> ws

    let pOperation =
        "operation" |> choiceL
            [ pstringCI "where" >>. pExpression |>> Where
              pstringCI "select" >>. sepBy1 (ws pIdentifier) (pchar ',') |>> Select ]

    let pPipeline = pipe2 (ws pIdentifier) (many (ws (pchar '|') >>. pOperation) .>> eof) (fun id ops -> { DataSource = id; Operations = ops })

let parse = FParsec.CharParsers.run Parser.pPipeline

module private Evaluation =
    open Query.Data

    module Result =
        let rec allOk xs =
            match xs with
            | [] -> Ok []
            | Ok x :: rest -> Result.map (fun r -> x::r) (allOk rest)
            | Error x :: _ -> Error x

    let errf fmt = Printf.ksprintf Error fmt
    let sources = [ "core.releases"; "core.runtimes"; "core.sdks"; "framework.releases"; "mono.releases" ]

    let evalCompOperator = function
        | Equal -> (=)
        | NotEqual -> (<>)
        | Less -> (<)
        | LessEqual -> (<=)
        | Greater -> (>)
        | GreaterEqual -> (>=)

    let evalBoolOperator = function
        | And -> (&&)
        | Or -> (||)

    let (|OOption|_|) object = 
        if object = null then
            Some None
        else
            let t = object.GetType()
            if t.IsGenericType && t.GetGenericTypeDefinition() = typedefof<_ option> 
            then Some (unbox<obj option> object)
            else None

    let (|OList|_|) object =
        let t = object.GetType()
        if t.IsGenericType && t.GetGenericTypeDefinition() = typedefof<_ list>
        then Some (unbox<obj list> object)
        else None

    let rec evalComparison op left right =
        let ok = box >> Ok
        match left, right with
        | l, r when l <> null && r <> null && l.GetType() = r.GetType() -> 
            evalCompOperator op (unbox l) (unbox r) |> ok
        | null, null ->
            ok (op = Equal || op = LessEqual || op = GreaterEqual)
        | OOption l, r -> 
            match l with
            | Some l -> evalComparison op l r
            | None -> ok (op = NotEqual)
        | l, OOption r -> 
            match r with
            | Some r -> evalComparison op l r
            | None -> ok (op = NotEqual)
        | OList list, other | other, OList list ->
            match op with
            | Equal -> list |> List.contains other |> ok
            | NotEqual -> list |> List.contains other |> not |> ok
            | _ -> errf "Comparison %A is not supported for lists" op
        | _ ->
            errf "Comparison is not supported between types %s and %s" (left.GetType().Name) (right.GetType().Name)

    let evalLiteral = function
        | NoneLiteral -> box None
        | StringLiteral s -> box s
        | VersionLiteral v -> box v
        | DateLiteral d -> box d

    let rec evalExpression data = function
        | Field name -> 
            match data |> Map.tryFind name with
            | Some x -> Ok x
            | None -> errf "The field '%s' does not exist. Available fields: %s" 
                       name (data |> Map.toList |> List.map fst |> String.concat ", ")
        | Literal lit -> 
            evalLiteral lit |> Ok
        | Negation expr ->
            match evalExpression data expr with
            | Ok b when (b :? bool) -> Ok (box (not (unbox b)))
            | Ok o -> errf "Value of type %s is not negatable" (o.GetType().Name)
            | Error e -> Error e
        | Comparison (lexpr, op, rexpr) ->
            match evalExpression data lexpr, evalExpression data rexpr with
            | Ok l, Ok r -> evalComparison op l r
            | Error e, _ | _, Error e -> Error e
        | BooleanExpression (lexpr, op, rexpr) ->
            match evalExpression data lexpr, evalExpression data rexpr with
            | Ok l, Ok r when (l :? bool) && (r :? bool) -> 
                evalBoolOperator op (unbox l) (unbox r) |> box |> Ok
            | Ok l, Ok r ->
                errf "Boolean operation requires two booleans, but got %s and %s" (l.GetType().Name) (r.GetType().Name)
            | Error e, _  | _, Error e -> 
                Error e

    let evalOperation = function
        | Select fields -> 
            Seq.map (Map.filter (fun key _ -> fields |> List.contains key)) >> Ok
        | Where expr -> 
            Seq.map (fun x -> 
                match evalExpression x expr with
                | Ok b when (b :? bool) -> Ok (unbox b)
                | Ok o -> errf "Expression needs to be of type bool, but is of type %s" (o.GetType().Name)
                | Error e -> Error e
                |> Result.map (fun b -> b, x))
            >> Seq.toList
            >> Result.allOk
            >> Result.map (Seq.ofList)
            >> Result.map (Seq.filter fst)
            >> Result.map (Seq.map snd)
        
    let evalOperations =
        List.fold (fun comp op -> comp >> Result.bind (evalOperation op)) Ok

    let evalPipeline pipeline =
        match pipeline.DataSource with
        | "core.releases" -> 
            let toMap (rel : Core.Release) =
                [ "version", box rel.Version
                  "date", box rel.ReleaseDate
                  "runtime", box (rel.Runtime |> Option.map box)
                  "sdk", box (rel.Sdks |> List.map box)
                  "asp-runtime", box (rel.AspRuntime |> Option.map box)
                  "cve", box (rel.Cves |> List.map box) ]
                |> Map.ofList
            let data = Core.releases |> List.map toMap
            evalOperations pipeline.Operations data
        // | "core.runtimes" -> Ok ()
        // | "core.sdks" -> Ok ()
        // | "framework.releases" -> Ok ()
        // | "mono.releases" -> Ok ()
        | invalid -> errf "Invalid data source '%s'" invalid

let evaluate = Evaluation.evalPipeline

let evaluateQuery query = 
    match parse query with
    | FParsec.CharParsers.Success (pipeline, _, _) ->
        evaluate pipeline
    | FParsec.CharParsers.Failure (mes, _, _) ->
        Error mes
