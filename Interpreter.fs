module Flox.Interpreter

open Flox.Types
open Flox.Parser

let rec eval expr : Primitive =
    match expr with
    | Empty -> Null
    | Literal v -> v
    | Grouping e -> eval e
    | Unary (op,expr) ->
        let right = eval expr
        match op.TokenType , right with
        | Minus , Num n  -> Num  -n
        | Bang  , Bool b -> Bool (not b)
        | _ -> Null
    | Binary (lhs,op,rhs) ->
        match eval lhs , eval rhs with
        | Null , Null ->
            match op.TokenType with
            | BangEqual  -> Bool false
            | EqualEqual -> Bool true
            | _ -> Null
        | Num l , Num r ->
            match op.TokenType with
            | Greater      -> l >  r |> Bool
            | GreaterEqual -> l >= r |> Bool
            | Less         -> l <  r |> Bool
            | LessEqual    -> l <= r |> Bool
            | BangEqual    -> l <> r |> Bool
            | EqualEqual   -> l =  r |> Bool
            | Plus         -> l +  r |> Num
            | Minus        -> l -  r |> Num
            | Star         -> l *  r |> Num
            | Slash        -> l /  r |> Num
            | _ -> Null
        | Str l , Str r ->
            match op.TokenType with
            | Plus       -> l +  r |> Str
            | BangEqual  -> l <> r |> Bool
            | EqualEqual -> l =  r |> Bool
            | _ -> Null
        | Bool l , Bool r ->
            match op.TokenType with
            | BangEqual  -> l <> r |> Bool
            | EqualEqual -> l =  r |> Bool
            | _ -> Null
        | _ -> Null

let interpret expr =
    eval expr