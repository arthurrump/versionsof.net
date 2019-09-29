module Query.Evaluator

open Data
open Language

/// Used to store the values of fields.
/// The int is used to store the index of the field, 
/// i.e. the order in which fields should be displayed.
type FieldMap = Map<string, int * obj>

module Result =
    let rec allOk xs =
        match xs with
        | [] -> Ok []
        | Ok x :: rest -> Result.map (fun r -> x::r) (allOk rest)
        | Error x :: _ -> Error x

let errf pos fmt = Printf.ksprintf (fun mes -> Error (pos, mes)) fmt

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
        | _ -> errf (getPosition expr) "Comparison %A is not supported for lists" op
    | _ ->
        errf (getPosition expr) "Comparison is not supported between types %s and %s" (typeName left) (typeName right)

let evalLiteral = function
    | NoneLiteral -> box None
    | StringLiteral s -> box s
    | VersionLiteral v -> box v
    | DateLiteral d -> box d

let rec evalExpression data expr =
    let errf fmt = errf (getPosition expr) fmt
    match expr.Value with
    | Field name -> 
        match data |> Map.tryFind name with
        | Some (_, x) -> Ok x
        | None -> errf "The field '%s' does not exist." name
    | Literal lit -> 
        evalLiteral lit |> Ok
    | Negation expr ->
        match evalExpression data expr with
        | Ok b when (b :? bool) -> Ok (box (not (unbox b)))
        | Ok o -> errf "Value of type %s is not negatable" (typeName o)
        | Error e -> Error e
    | Comparison (lexpr, op, rexpr) ->
        match evalExpression data lexpr, evalExpression data rexpr with
        | Ok l, Ok r -> evalComparison expr op l r 
        | Error e, _ | _, Error e -> Error e
    | BooleanExpression (lexpr, op, rexpr) ->
        match evalExpression data lexpr, evalExpression data rexpr with
        | Ok l, Ok r when (l :? bool) && (r :? bool) -> 
            evalBoolOperator op (unbox l) (unbox r) |> box |> Ok
        | Ok l, Ok r ->
            errf "Boolean operation requires two booleans, but got %s and %s" (typeName l) (typeName r)
        | Error e, _  | _, Error e -> 
            Error e

let evalOperation op = 
    match op.Value with
    | Select fields -> 
        Seq.map (fun fieldMap ->
            [ for f in fields -> 
                match fieldMap |> Map.tryFind f.Value with
                | Some (_, x) -> Ok (f.Value, x)
                | None -> errf (getPosition f) "The field '%s' does not exist." f.Value
            ] |> Result.allOk)
        >> Seq.toList
        >> Result.allOk
        >> Result.map (Seq.map toFieldMap)
    | Where expr -> 
        Seq.map (fun fieldMap -> 
            match evalExpression fieldMap expr with
            | Ok b when (b :? bool) -> Ok (unbox b)
            | Ok o -> errf (getPosition expr) "Expression needs to be of type bool, but is of type %s" (typeName o)
            | Error e -> Error e
            |> Result.map (fun b -> b, fieldMap))
        >> Seq.toList
        >> Result.allOk
        >> Result.map (Seq.filter fst >> Seq.map snd)
    | SortBy (desc, expr) ->
        let sortBy = if desc then Seq.sortByDescending else Seq.sortBy
        Seq.map (fun fieldMap -> evalExpression fieldMap expr |> Result.map (fun ex -> (ex, fieldMap)))
        >> Seq.toList
        >> Result.allOk
        >> Result.map (sortBy (fst >> unbox) >> Seq.map snd)
    
let evalOperations =
    List.fold (fun comp op -> comp >> Result.bind (evalOperation op)) Ok

let evalPipeline data pipeline =
    evalOperations pipeline.Operations data
