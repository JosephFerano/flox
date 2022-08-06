module Flox.Parser

open Flox.Types

type Expr =
    | Empty
    | Unary of operator : Token * Expr
    | Binary of lhs : Expr * operator : Token * rhs : Expr
    | Grouping of Expr
    | Literal of obj

type ParseContext = {
    Current : int
    Tokens : Token array
    Expression : Expr
}

let peek ctx = ctx.Tokens[ctx.Current]
let addExpression ctx expr = { ctx with Expression = expr }
let advance ctx = { ctx with Current = ctx.Current + 1 }
let addExprAdvance expr = addExpression expr >> advance

let matchTokenTypes ctx (tokenTypes : TokenType list) =
    ctx.Current < ctx.Tokens.Length
    && tokenTypes |> List.contains ((peek ctx).TokenType)

let rec binaryDescend tokenTypes descend leftCtx =
    if matchTokenTypes leftCtx tokenTypes then
        let rightCtx = leftCtx |> advance |> descend
        
        Binary(leftCtx.Expression, peek leftCtx, rightCtx.Expression)
        |> addExpression rightCtx
        |> binaryDescend tokenTypes descend
    else
        leftCtx
        
let rec expression (ctx : ParseContext) = equality ctx

and equality   ctx = ctx |> comparison |> binaryDescend [ BangEqual ; EqualEqual ] comparison
and comparison ctx = ctx |> term       |> binaryDescend [ Greater ; GreaterEqual ; Less ; LessEqual ] term
and term       ctx = ctx |> factor     |> binaryDescend [ Plus ; Minus ] factor
and factor     ctx = ctx |> unary      |> binaryDescend [ Slash ; Star ] unary

and unary ctx =
    if matchTokenTypes ctx [ Bang ; Minus ] then
        let rightCtx = ctx |> advance |> unary
        Unary(peek ctx, rightCtx.Expression) |> addExpression rightCtx
    else primary ctx
    
and primary ctx =
    let token = peek ctx
    match token.TokenType with
    | False   -> Literal false |> addExprAdvance ctx
    | True    -> Literal true  |> addExprAdvance ctx
    | Nil     -> Literal null  |> addExprAdvance ctx
    | String  -> Literal $"\"{token.Lexeme}\"" |> addExprAdvance ctx
    | Number  -> Literal (token.Literal |> Option.defaultValue 0.0) |> addExprAdvance ctx
    | LeftParen ->
        let ctx' = ctx |> advance |> expression 
        match (peek ctx').TokenType with
        | RightParen -> Grouping(ctx'.Expression) |> addExprAdvance ctx'
        | _ -> ctx'
    | _ -> ctx

let parseTokens tokens =
    { Current = 0
      Tokens = tokens |> List.toArray
      Expression = Empty }
    |> expression
    |> fun ctx -> ctx.Expression
        
let rec prettyPrint expressions =
    let parenthesize s = $"({s})"
    match expressions with
    | Empty -> "()"
    | Literal o -> if o = null then "nil" else string o
    | Grouping exp ->
        let expStr = prettyPrint exp
        parenthesize $"group {expStr}"
    | Unary (op,expr) ->
        let expStr = prettyPrint expr
        parenthesize $"{op.Lexeme} {expStr}"
    | Binary (lhs,op,rhs) ->
        let lhsStr = prettyPrint lhs
        let rhsStr = prettyPrint rhs
        parenthesize $"{op.Lexeme} {lhsStr} {rhsStr}"

// let e5 = Binary(Unary("-",Literal(123)),"*",Grouping(Literal(45.67)))
// let e1 = Binary ((Literal 5) , "+" , (Literal 3))
// let e2 = Grouping (Literal 5)
// let e3 = Literal 5
// let e4 = Unary ("!" , (Literal 10))
// [ e1 ; e2 ; e3 ; e4 ; e5] |> List.map (prettyPrint (StringBuilder()))
