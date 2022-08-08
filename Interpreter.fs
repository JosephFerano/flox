module Flox.Interpreter

open Utilities
open Types
open Parser

type RuntimeError = {
    Token : Token
    Message : string
}
    
let error token message = Error { Token = token ;  Message = message }

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
            | _ -> error op $"Cannot use ({op.Lexeme}) operator one or both operands are nil"
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
            | Slash        -> l /  r |> Num  |> Ok
            | _ -> error op $"Cannot use ({op.Lexeme}) operator on numbers"
        | Ok (Str l) , Ok (Str r) ->
            match op.TokenType with
            | Plus       -> l +  r |> Str  |> Ok
            | BangEqual  -> l <> r |> Bool |> Ok
            | EqualEqual -> l =  r |> Bool |> Ok
            | _ -> error op $"Cannot use ({op.Lexeme}) operator on strings"
        | Ok (Bool l) , Ok (Bool r) ->
            match op.TokenType with
            | BangEqual  -> l <> r |> Bool |> Ok
            | EqualEqual -> l =  r |> Bool |> Ok
            | _ -> error op $"Cannot use ({op.Lexeme}) operator on booleans"
        | Ok lp , Ok rp ->
            error op $"Cannot use ({op.Lexeme}) operator when types are different: left {Primitive.getTypeName lp} , right {Primitive.getTypeName rp}"
        | Error e , _ 
        | _ , Error e -> Error e

let interpret expr =
    eval expr