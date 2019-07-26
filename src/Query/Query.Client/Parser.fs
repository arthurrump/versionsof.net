module Query

open FParsec
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
    | Negation of Expression
    | Field of string
    | Literal of Literal

and CompOperator = Equal | NotEqual | Less | LessEqual | Greater | GreaterEqual

and Literal =
    | StringLiteral of string
    | VersionLiteral of Version
    | DateLiteral of DateTime

let notSpaces = isNoneOf [ ' '; '\t'; '\r'; '\n' ]
let ws p = spaces >>. p .>> spaces

let pIdentifier = many1SatisfyL isAsciiLetter "identifier"

let pStringLiteral = between (pchar '"') (pchar '"') (manySatisfy ((<>) '"')) |>> StringLiteral <?> "a string"
let pVersionLiteral = pipe3 pint32 (many (pchar '.' >>. pint32)) (opt (pchar '-' >>. manySatisfy notSpaces)) (fun n ns pre -> VersionLiteral { Numbers = n::ns; Preview = pre }) <?> "a version"
let pDateLiteral = pipe3 pint32 (pchar '-' >>. pint32) (pchar '-' >>. pint32) (fun y m d -> DateLiteral (DateTime(y, m, d))) <?> "a date (yyyy-mm-dd)"
let pLiteral = pStringLiteral <|> pVersionLiteral <|> pDateLiteral

let pCompOperator = 
    "comparison operator" |> choiceL 
        [ pstring "="  >>% Equal
          pstring "==" >>% Equal
          pstring "!=" >>% NotEqual
          pstring "<>" >>% NotEqual
          pstring "<"  >>% Less
          pstring "<=" >>% LessEqual
          pstring ">"  >>% Greater
          pstring ">=" >>% GreaterEqual ]

let (<!>) (p: Parser<_,_>) label : Parser<_,_> =
    fun stream ->
        printfn "%A: Entering %s" stream.Position label
        let reply = p stream
        printfn "%A: Leaving %s (%A)" stream.Position label reply.Status
        reply

let pExpression, pExpressionImpl = createParserForwardedToRef()
let pNegation = (pstring "!" <|> (pstringCI "not" .>> spaces1)) >>. pExpression |>> Negation
let pBasicExpression =
    choice 
        [ pNegation <!> "negation"
          (pIdentifier |>> Field) <!> "identifier"
          (pLiteral |>> Literal) <!> "literal"
          between (pchar '(') (pchar ')') pExpression ]
    |> ws

let pComparison = pipe3 pBasicExpression pCompOperator pExpression (fun ex1 op ex2 -> Comparison (ex1, op, ex2))
pExpressionImpl :=
    "expression" |> choiceL 
        [ attempt pComparison <!> "comparison"
          pBasicExpression ]
    |> ws

let pOperation =
    "operation" |> choiceL
        [ pstringCI "where" >>. pExpression |>> Where
          pstringCI "select" >>. sepBy1 (ws pIdentifier) (pchar ',') |>> Select ]

let pPipeline = pipe2 (ws pIdentifier) (many (ws (pchar '|') >>. pOperation) .>> eof) (fun id ops -> { DataSource = id; Operations = ops })

let parsePipeline text = run pPipeline text

[<EntryPoint>]
let main argv =
    parsePipeline "sdks | where version > 1.2.3-hello\r\n | where \tsdk = runtime = security   \t| select runtime, date"
    |> printfn "%A"

    0
