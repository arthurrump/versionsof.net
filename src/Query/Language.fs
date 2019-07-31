module Query.Language

open System
open NetCore.Versions
open FsToolkit.ErrorHandling.AsyncResultCEExtensions
open System.Net.Http
open System.Collections.Generic
open Thoth.Json.Net

type Pipeline = 
    { DataSource : string
      Operations : Operation list }

and Operation =
    | Where of Expression
    | Select of fields : string list
    | SortBy of descending: bool * Expression

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

[<RequireQualifiedAccess>]
type Token =
    | None
    | AtPosition of FParsec.Position
    | DataSource
    | Operation of Operation
    | Expression of Expression
    | Literal of Literal

type CodeStyle = Cs | Fs | Vb

type FieldMap = Map<string, int * obj>

type SourceMap = Map<Token, FParsec.Position * FParsec.Position>

module private Parser =
    open FParsec

    let ws p = spaces >>. p .>> spaces
    let srcMap tkn p : Parser<'a, SourceMap> = 
        pipe3 getPosition p getPosition (fun prePos res postPos -> res, (prePos, postPos))
        >>= fun (res, pos) -> updateUserState (Map.add (tkn res) pos) >>% res

    let pIdentifier =  many1Satisfy2L isAsciiLetter (fun c -> isAsciiLetter c || c = '.') "an identifier"

    let pNoneLiteral = 
        "'none'" |> choiceL 
            [ pstringCI "none" 
              pstringCI "null" 
              pstringCI "nothing" ] 
        >>% NoneLiteral

    let pStringLiteral = 
        let normal = manySatisfy (fun c -> c <> '\\' && c <> '"')
        let escaped = pstring "\\" >>. anyOf "\\\"" |>> string
        between (pstring "\"") (pstring "\"")
            (stringsSepBy normal escaped)
        |>> StringLiteral

    let pDateLiteral = 
        pipe3 pint32 (pchar '-' >>. pint32) (pchar '-' >>. pint32) 
              (fun y m d -> DateLiteral (DateTime(y, m, d))) 
        <?> "a date (yyyy-mm-dd)"

    let pVersionLiteral = 
        sepBy1 pint32 (pchar '.') 
        .>>. opt (pchar '-' >>. many1Satisfy (fun c -> isAsciiLetter c || isDigit c || c = '-' || c = '_' || c = '.'))
        |>> fun (ns, pre) -> VersionLiteral { Numbers = ns; Preview = pre }
        <?> "a version"

    let pLiteral = pNoneLiteral <|> pStringLiteral <|> attempt pDateLiteral <|> pVersionLiteral |> srcMap Token.Literal

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
    let pCompExpr, pCompExprImpl = createParserForwardedToRef()

    let pNegation =
        (pstring "!" <|> attempt (pstringCI "not" .>> followedBy (satisfy (not << isLetter)))) 
        >>. pExpression 
        |>> Negation

    let pBasicExpr =
        ws <| choice 
            [ pNegation
              between (pchar '(') (pchar ')') pExpression 
              pLiteral |>> Literal
              pIdentifier |>> Field ]
        |> srcMap Token.Expression
    
    do pCompExprImpl := 
        pBasicExpr .>>. opt (pCompOperator .>>. pCompExpr)
        |>> fun (expr, op) ->
            match op with
            | Some (operator, other) -> Comparison (expr, operator, other)
            | None -> expr
        |> srcMap Token.Expression

    do pExpressionImpl :=
        pCompExpr .>>. opt (pBoolOperator .>>. pExpression)
        |>> fun (expr, op) ->
            match op with
            | Some (operator, other) -> BooleanExpression (expr, operator, other)
            | None -> expr
        |> srcMap Token.Expression

    let pSortBy =
        (pstringCI "sortby" <|> pstringCI "orderby") 
        >>. (opt (pstringCI "descending") |>> Option.isSome) 
        .>>. pExpression 
        |>> SortBy

    let pOperation =
        "operation" |> choiceL
            [ pstringCI "where" >>. pExpression |>> Where
              pstringCI "select" >>. sepBy1 (ws pIdentifier) (pchar ',') |>> Select
              pSortBy ]
        |> srcMap Token.Operation

    let pPipeline = 
        pipe2 (ws pIdentifier |> srcMap (fun _ -> Token.DataSource)) 
              (many (ws (pchar '|') >>. pOperation) .>> eof) 
              (fun id ops -> { DataSource = id; Operations = ops })

let parse = FParsec.CharParsers.runParserOnString Parser.pPipeline Map.empty ""

module PrettyPrint =
    let prettyLiteral style = function
        | NoneLiteral -> match style with Cs -> "null" | Fs -> "None" | Vb -> "Nothing"
        | StringLiteral s -> "\"" + s.Replace("\\", "\\\\").Replace("\"", "\\\"") + "\""
        | VersionLiteral v -> string v
        | DateLiteral d -> d.ToString "yyyy-MM-dd"

    let prettyCompOperator style = function
        | Equal -> match style with Cs -> "==" | Fs | Vb -> "="
        | NotEqual -> match style with Cs -> "!=" | Fs | Vb -> "<>"
        | Less -> "<"
        | LessEqual -> "<="
        | Greater -> ">"
        | GreaterEqual -> ">="

    let prettyBoolOperator style = function
        | And -> match style with Cs | Fs -> "&&" | Vb -> "And"
        | Or -> match style with Cs | Fs -> "||" | Vb -> "Or"

    let rec prettyExpression style = function
        | Field name -> 
            name
        | Literal lit -> 
            prettyLiteral style lit
        | Negation expr -> 
            (match style with Cs -> "(!" | Fs -> "(not " | Vb -> "(Not ") 
            + prettyExpression style expr 
            + ")"
        | BooleanExpression (l, op, r) ->
            sprintf "(%s %s %s)" (prettyExpression style l) (prettyBoolOperator style op) (prettyExpression style r)
        | Comparison (l, op, r) ->
            sprintf "(%s %s %s)" (prettyExpression style l) (prettyCompOperator style op) (prettyExpression style r)

    let prettyOperation style = function
        | Select fields -> 
            "| " + (match style with Cs | Vb -> "Select" | Fs -> "select") + " "
            + (fields |> String.concat ", ")
        | Where expr ->
            "| " + (match style with Cs | Vb -> "Where" | Fs -> "where") + " "
            + (prettyExpression style expr)
        | SortBy (desc, expr) ->
            "| " + (match style with Cs | Vb -> "OrderBy" | Fs -> "sortBy") 
            + (if desc then "Descending " else " ")
            + (prettyExpression style expr)

    let prettyPipeline style pipeline =
        pipeline.DataSource + "\n" 
        + (pipeline.Operations |> List.map (prettyOperation style) |> String.concat "\n")

module Evaluation =
    open Query.Data

    module Result =
        let rec allOk xs =
            match xs with
            | [] -> Ok []
            | Ok x :: rest -> Result.map (fun r -> x::r) (allOk rest)
            | Error x :: _ -> Error x

    let errf tkn fmt = Printf.ksprintf (fun mes -> Error (tkn, mes)) fmt
    
    let dataUrl (dataSource : string) = 
        sprintf "/query/%s.json" (dataSource.Split('.') |> String.concat "/")

    type DataCache(http : HttpClient) =
        let cache = new Dictionary<string, string>()
        member __.Get dataSource =
            asyncResult {
                match cache.TryGetValue dataSource with
                | (true, res) -> 
                    return res
                | (false, _) ->
                    let! res = dataUrl dataSource |> http.GetAsync |> Async.AwaitTask
                    if res.IsSuccessStatusCode then
                        let! body = res.Content.ReadAsStringAsync() |> Async.AwaitTask
                        cache.Add (dataSource, body)
                        return body
                    else
                        return! errf Token.None "Request for %s failed with status code %A" dataSource res.StatusCode
            }

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
        if object = null then 
            None
        else 
            let t = object.GetType()
            if t.IsGenericType && t.GetGenericTypeDefinition() = typedefof<_ list>
            then Some (unbox<obj list> object)
            else None

    let typeName obj = if obj <> null then obj.GetType().Name else "Option _"

    let rec evalComparison expr op left right =
        let ok = box >> Ok
        match left, right with
        | l, r when l <> null && r <> null && l.GetType() = r.GetType() -> 
            evalCompOperator op (unbox l) (unbox r) |> ok
        | null, null ->
            ok (op = Equal || op = LessEqual || op = GreaterEqual)
        | OOption l, r -> 
            match l with
            | Some l -> evalComparison expr op l r
            | None -> ok (op = NotEqual)
        | l, OOption r -> 
            match r with
            | Some r -> evalComparison expr op l r
            | None -> ok (op = NotEqual)
        | OList list, other | other, OList list when list.GetType().GetGenericArguments().[0] = other.GetType() ->
            match op with
            | Equal -> list |> List.contains other |> ok
            | NotEqual -> list |> List.contains other |> not |> ok
            | _ -> errf (Token.Expression expr) "Comparison %A is not supported for lists" op
        | _ ->
            errf (Token.Expression expr) "Comparison is not supported between types %s and %s" (typeName left) (typeName right)

    let evalLiteral = function
        | NoneLiteral -> box None
        | StringLiteral s -> box s
        | VersionLiteral v -> box v
        | DateLiteral d -> box d

    let rec evalExpression data = function
        | Field name as expr -> 
            match data |> Map.tryFind name with
            | Some (_, x) -> Ok x
            | None -> errf (Token.Expression expr) "The field '%s' does not exist. Available fields: %s" 
                       name (data |> Map.toList |> List.map fst |> String.concat ", ")
        | Literal lit -> 
            evalLiteral lit |> Ok
        | Negation expr as neg ->
            match evalExpression data expr with
            | Ok b when (b :? bool) -> Ok (box (not (unbox b)))
            | Ok o -> errf (Token.Expression neg) "Value of type %s is not negatable" (typeName o)
            | Error e -> Error e
        | Comparison (lexpr, op, rexpr) as expr ->
            match evalExpression data lexpr, evalExpression data rexpr with
            | Ok l, Ok r -> evalComparison expr op l r 
            | Error e, _ | _, Error e -> Error e
        | BooleanExpression (lexpr, op, rexpr) as expr ->
            match evalExpression data lexpr, evalExpression data rexpr with
            | Ok l, Ok r when (l :? bool) && (r :? bool) -> 
                evalBoolOperator op (unbox l) (unbox r) |> box |> Ok
            | Ok l, Ok r ->
                errf (Token.Expression expr) "Boolean operation requires two booleans, but got %s and %s" (typeName l) (typeName r)
            | Error e, _  | _, Error e -> 
                Error e

    let evalOperation = function
        | Select fields as op -> 
            Seq.map (fun fieldMap ->
                [ for f in fields -> 
                    match fieldMap |> Map.tryFind f with
                    | Some (_, x) -> Ok (f, x)
                    | None -> errf (Token.Operation op) "The field '%s' does not exist. Available fields: %s" 
                                f (fieldMap |> Map.toList |> List.map fst |> String.concat ", ")
                ] |> Result.allOk)
            >> Seq.toList
            >> Result.allOk
            >> Result.map (Seq.map fieldMap)
        | Where expr as op -> 
            Seq.map (fun x -> 
                match evalExpression x expr with
                | Ok b when (b :? bool) -> Ok (unbox b)
                | Ok o -> errf (Token.Operation op) "Expression needs to be of type bool, but is of type %s" (typeName o)
                | Error e -> Error e
                |> Result.map (fun b -> b, x))
            >> Seq.toList
            >> Result.allOk
            >> Result.map (Seq.filter fst)
            >> Result.map (Seq.map snd)
        | SortBy (desc, expr) ->
            let sortBy = if desc then Seq.sortByDescending else Seq.sortBy
            Seq.map (fun fm -> evalExpression fm expr |> Result.map (fun ex -> (ex, fm)))
            >> Seq.toList
            >> Result.allOk
            >> Result.map (sortBy (fst >> unbox) >> Seq.map snd)
        
    let evalOperations =
        List.fold (fun comp op -> comp >> Result.bind (evalOperation op)) Ok

    let evalPipeline (dataCache : DataCache) pipeline =
        let eval' fieldmap = List.map fieldmap >> evalOperations pipeline.Operations
        let eval decoder fieldmap =
            asyncResult {
                let! json = dataCache.Get pipeline.DataSource
                let! data = Decode.fromString (Decode.list decoder) json |> Result.mapError (fun mes -> Token.None, mes)
                return! data |> eval' fieldmap
            }

        match pipeline.DataSource with
        | "core.releases" -> eval Core.Release.Decoder Core.Release.FieldMap
        | "core.runtimes" -> eval Core.Runtime.Decoder Core.Runtime.FieldMap
        | "core.sdks" -> eval Core.Sdk.Decoder Core.Sdk.FieldMap
        | "framework.releases" -> eval Framework.Release.Decoder Framework.Release.FieldMap
        | "mono.releases" -> eval Mono.Release.Decoder Mono.Release.FieldMap
        | invalid -> async { return errf Token.DataSource "Invalid data source '%s'" invalid }

let evaluate dc = Evaluation.evalPipeline dc

let evaluateQuery dc query = 
    match parse query with
    | FParsec.CharParsers.Success (pipeline, srcMap, _) ->
        async { let! res = evaluate dc pipeline in return res, srcMap }
    | FParsec.CharParsers.Failure (mes, err, srcMap) ->
        async { return Error (Token.AtPosition err.Position, mes), srcMap }
