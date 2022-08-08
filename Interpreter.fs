module Flox.Interpreter

open Utilities
open Types
open Parser

type RuntimeError = {
    Token : Token
    Message : string
}
    
let error token message = Error { Token = token ;  Message = message }

let printPrimitive = function
    | Null -> "nil"
    | Num n ->
        let sn = string n
        if sn.EndsWith(".0")
            then $"{int n}"
            else $"{n}"
    | Str s -> s
    | Bool b -> if b then "true" else "false"

let rec private eval expr : Result<Primitive, RuntimeError> =
    match expr with
    | Empty -> Ok Null
    | Literal v -> Ok v
    | Grouping e -> eval e
    | Unary (op,expr) ->
        let unary right =
            match op.TokenType , right with
            | Minus , Num n  -> Num  -n |> Ok
            | Bang  , Bool b -> Bool (not b) |> Ok
            | _     , prim -> error op $"Cannot apply unary ({op.Lexeme}) operator to type {Primitive.getTypeName prim}"
        eval expr >>= unary
    | Binary (lhs,op,rhs) ->
        match eval lhs , eval rhs with
        | Ok (Null) , _
        | _         , Ok (Null) ->
            match op.TokenType with
            | BangEqual  -> Bool false |> Ok
            | EqualEqual -> Bool true |> Ok
            | _ -> error op $"Cannot use ({op.Lexeme}) operator when one or both operands are nil"
        | Ok (Num l) , Ok (Num r) ->
            match op.TokenType with
            | Greater      -> l >  r |> Bool |> Ok
            | GreaterEqual -> l >= r |> Bool |> Ok
            | Less         -> l <  r |> Bool |> Ok
            | LessEqual    -> l <= r |> Bool |> Ok
            | BangEqual    -> l <> r |> Bool |> Ok
            | EqualEqual   -> l =  r |> Bool |> Ok
            | Plus         -> l +  r |> Num  |> Ok
            | Minus        -> l -  r |> Num  |> Ok
            | Star         -> l *  r |> Num  |> Ok
            | Slash        ->
                if r <> 0.0 then
                    l /  r |> Num  |> Ok
                else
                    error op $"Division by zero"
            | _ -> error op $"Cannot use ({op.Lexeme}) operator on numbers"
        | Ok (Str l) , Ok (Str r) ->
            match op.TokenType with
            | Plus       -> l +  r |> Str  |> Ok
            | BangEqual  -> l <> r |> Bool |> Ok
            | EqualEqual -> l =  r |> Bool |> Ok
            | _ -> error op $"Cannot use ({op.Lexeme}) operator on strings"
        | Ok (Str ls) , Ok rp ->
            match op.TokenType with
            | Plus       -> ls + (printPrimitive rp) |> Str |> Ok
            | _ -> error op $"Cannot use ({op.Lexeme}) operator when left operand is a string"
        | Ok lp , Ok (Str rs) ->
            match op.TokenType with
            | Plus       -> (printPrimitive lp) + rs |> Str |> Ok
            | _ -> error op $"Cannot use ({op.Lexeme}) operator when right operand is a string"
        | Ok (Bool l) , Ok (Bool r) ->
            match op.TokenType with
            | BangEqual  -> l <> r |> Bool |> Ok
            | EqualEqual -> l =  r |> Bool |> Ok
            | _ -> error op $"Cannot use ({op.Lexeme}) operator on booleans"
        | Ok lp , Ok rp ->
            let types = $"left {Primitive.getTypeName lp} , right {Primitive.getTypeName rp}"
            error op $"Cannot use ({op.Lexeme}) operator when types are different: {types}"
        | Error e , _ 
        | _ , Error e -> Error e

let interpret expr =
    eval expr