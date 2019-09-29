module Query.Language

open System
open NetCore.Versions

// Annotated AST Helpers
////////////////////////
type Annotated<'t, 'a> =
    { Value : 't
      Annotation : 'a }

let toPositioned start value ent = { Value = value; Annotation = (start, ent) }

type PosPair = FParsec.Position * FParsec.Position

[<RequireQualifiedAccess>]
type Position =
    | None
    | Single of FParsec.Position
    | Range of PosPair

let getPosition pos = Position.Range pos.Annotation

// AST
//////
type Pipeline<'a> = 
    { DataSource : Annotated<string, 'a>
      Operations : Annotated<Operation<'a>, 'a> list }

and Operation<'a> =
    | Where of Annotated<Expression<'a>, 'a>
    | Select of fields : Annotated<string, 'a> list
    | SortBy of descending: bool * Annotated<Expression<'a>, 'a>

and Expression<'a> =
    | Comparison of Annotated<Expression<'a>, 'a> * CompOperator * Annotated<Expression<'a>, 'a>
    | BooleanExpression of Annotated<Expression<'a>, 'a> * BoolOperator * Annotated<Expression<'a>, 'a>
    | Negation of Annotated<Expression<'a>, 'a>
    | Field of string
    | Literal of Literal

and CompOperator = Equal | NotEqual | Less | LessEqual | Greater | GreaterEqual
and BoolOperator = And | Or

and Literal =
    | NoneLiteral
    | StringLiteral of string
    | VersionLiteral of Version
    | DateLiteral of DateTime

// Parser definition
////////////////////
module private Parser =
    open FParsec

    let ws p = spaces >>. p .>> spaces
    let keyword str = attempt (pstringCI str .>> followedBy (skipSatisfy (not << isLetter) <|> eof)) <?> "'" + str + "'"
    let positioned p = pipe3 getPosition p getPosition toPositioned

    let pIdentifier =  many1Satisfy2L isAsciiLetter (fun c -> isAsciiLetter c || c = '.') "an identifier"

    let pNoneLiteral = 
        "'none'" |> choiceL 
            [ keyword "none" 
              keyword "null" 
              keyword "nothing" ] 
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

    let pLiteral = pNoneLiteral <|> pStringLiteral <|> attempt pDateLiteral <|> pVersionLiteral

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
            [ keyword "and" >>% And
              pstring "&&"  >>% And
              keyword "or"  >>% Or
              pstring "||"  >>% Or ]

    let pExpression, pExpressionImpl = createParserForwardedToRef()
    let pCompExpr, pCompExprImpl = createParserForwardedToRef()

    let pNegation =
        (pstring "!" <|> keyword "not") 
        >>. pExpression 
        |>> Negation

    let pBasicExpr =
        ws <| choice 
            [ pNegation |> positioned
              between (pchar '(') (pchar ')') pExpression 
              pLiteral |>> Literal |> positioned
              pIdentifier |>> Field |> positioned ]
        <?> "an expression"
    
    do pCompExprImpl := 
        pBasicExpr .>>. opt (pCompOperator .>>. pCompExpr)
        |>> fun (expr, op) ->
            match op with
            | Some (operator, other) -> Comparison (expr, operator, other)
            | None -> expr.Value
        |> positioned
        <?> "an expression"

    do pExpressionImpl :=
        pCompExpr .>>. opt (pBoolOperator .>>. pExpression)
        |>> fun (expr, op) ->
            match op with
            | Some (operator, other) -> BooleanExpression (expr, operator, other)
            | None -> expr.Value
        |> positioned
        <?> "an expression"

    let pSortBy =
        attempt (
            (pstringCI "sortby" <|> pstringCI "orderby") 
            >>. (opt (pstringCI "descending") |>> Option.isSome) 
            .>> followedBy (satisfy (not << isLetter)))
        .>>. pExpression 
        |>> SortBy

    let pOperation =
        "operation" |> choiceL
            [ keyword "where" >>. pExpression |>> Where
              keyword "select" >>. sepBy1 (ws (positioned pIdentifier)) (pchar ',') |>> Select
              pSortBy ]
        |> positioned

    let pPipeline = 
        pipe2 (ws (positioned pIdentifier))
              (many (ws (pchar '|') >>. pOperation) .>> eof) 
              (fun id ops -> { DataSource = id; Operations = ops })

/// Parse the language to a Pipeline, annotated with the location of each token
let parse = FParsec.CharParsers.runParserOnString Parser.pPipeline () ""

// Pretty printing
//////////////////
module PrettyPrint =
    type CodeStyle = Cs | Fs | Vb

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
            + prettyExpression style expr.Value
            + ")"
        | BooleanExpression (l, op, r) ->
            sprintf "(%s %s %s)" (prettyExpression style l.Value) (prettyBoolOperator style op) (prettyExpression style r.Value)
        | Comparison (l, op, r) ->
            sprintf "(%s %s %s)" (prettyExpression style l.Value) (prettyCompOperator style op) (prettyExpression style r.Value)

    let prettyOperation style = function
        | Select fields -> 
            "| " + (match style with Cs | Vb -> "Select" | Fs -> "select") + " "
            + (fields |> List.map (fun f -> f.Value) |> String.concat ", ")
        | Where expr ->
            "| " + (match style with Cs | Vb -> "Where" | Fs -> "where") + " "
            + (prettyExpression style expr.Value)
        | SortBy (desc, expr) ->
            "| " + (match style with Cs | Vb -> "OrderBy" | Fs -> "sortBy") 
            + (if desc then "Descending " else " ")
            + (prettyExpression style expr.Value)

    let prettyPipeline style pipeline =
        pipeline.DataSource.Value + "\n" 
        + (pipeline.Operations |> List.map (fun op -> op.Value |> prettyOperation style) |> String.concat "\n")
