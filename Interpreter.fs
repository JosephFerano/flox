module Flox.Interpreter

open System
open Flox.Types
open Flox.Parser

let isTruthy (o : obj) =
    match o with
    | null -> false
    | :? bool as b -> b
    | _ -> true

let isEqual (lhs : obj) (rhs : obj) =
    if lhs = null && rhs = null then
        true
    else
        match lhs , rhs with
        | (:? string as l), (:? string as r) -> l = r
        | (:? float  as l), (:? float  as r) -> l = r
        | (:? bool   as l), (:? bool   as r) -> l = r
        | _ -> false

let rec eval expr : obj =
    match expr with
    | Empty -> null
    | Literal v -> v
    | Grouping e -> eval e
    | Unary (op,expr) ->
        let right = eval expr
        match op.TokenType with
        | Minus -> -(unbox<float> right)
        | Bang -> isTruthy
        | _ -> null
    | Binary (lhs,op,rhs) ->
        let left = eval lhs
        let right = eval rhs
        match op.TokenType with
        | Greater      -> unbox<float> left >  unbox<float> right |> box
        | GreaterEqual -> unbox<float> left >= unbox<float> right |> box
        | Less         -> unbox<float> left <  unbox<float> right |> box
        | LessEqual    -> unbox<float> left <= unbox<float> right |> box
        | Minus -> unbox<float> left - unbox<float> right |> box
        | Star ->  unbox<float> left * unbox<float> right |> box
        | Slash -> unbox<float> left / unbox<float> right |> box
        | Plus ->
            match left , right with
            | (:? string as l), (:? string as r) -> l + r |> box
            | (:? float  as l), (:? float  as r) -> l + r |> box
            | _ -> null
        | BangEqual -> isEqual left right
        | EqualEqual -> isEqual left right
        | _ -> null

let interpret expr =
    eval expr