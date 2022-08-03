module Flox.Scanner

open Flox.Types
open Flox.Utilities

let reserved =
    [ "and" , And
      "class" , Class
      "else" , Else
      "false" , False
      "For" , For
      "fun" , Fun
      "if" , If
      "nil" , Nil
      "or" , Or
      "print" , Print
      "return" , Return
      "super" , Super
      "this" , This
      "true" , True
      "var" , Var
      "while" , While ]
    |> Map.ofSeq
    
type ScanContext = {
    Source : char array
    Start : int
    Current : int
    LineNum : int
    Tokens : Token list
    FoundError : bool
}

let newToken ctx tokenType = {
    TokenType = tokenType
    Line = ctx.LineNum
    Literal = None
    Lexeme = ctx.Source[ctx.Start .. ctx.Current] |> string
}
    
let newNumToken ctx =
    let num = ctx.Source[ctx.Start .. ctx.Current] |> string
    { TokenType = Number
      Line = ctx.LineNum
      Literal = Some (float num)
      Lexeme = num }
    
let isAtEnd ctx = ctx.Current >= ctx.Source.Length
let advance ctx = { ctx with Current = ctx.Current + 1 }
let peek ctx = if isAtEnd ctx then None else Some ctx.Source[ctx.Current]
let peekNext ctx = if isAtEnd { ctx with Current = ctx.Current + 1 } then None else Some ctx.Source[ctx.Current + 1]
let checkNextChar ctx expected = not (isAtEnd ctx) && (ctx |> peek |> Option.mapWithDefault ((=) expected) false)

let isDigit c = c >= '0' && c <= '9'
let (|Digit|_|) (c : char) = if isDigit c then Some () else None
    
let rec numericLiteral ctx =
    match peek ctx with
    | Some Digit -> ctx |> advance |> numericLiteral
    | Some c when c = '.' ->
        if peekNext ctx |> Option.mapWithDefault isDigit false
            then ctx |> advance |> numericLiteral
            else ctx |> numericLiteral
    | Some _
    | None -> Ok { ctx with Tokens = newNumToken ctx::ctx.Tokens ; Current = ctx.Current + 1 ; Start = ctx.Current + 1 }

let rec stringLiteral ctx =
    if isAtEnd ctx then
        Error "Unterminated string."
    else
        match ctx.Source[ctx.Current] with
        | c when c = '"' ->
            let token = { TokenType = TokenType.String
                          Lexeme = ctx.Source[ctx.Start .. ctx.Current - 1] |> string
                          Literal = None
                          Line = ctx.LineNum }
            Ok { ctx with Tokens = token::ctx.Tokens ; Current = ctx.Current + 1 ; Start = ctx.Current + 1 }
        | c when c = '\n' ->
            let ctx' = { ctx with LineNum = ctx.LineNum + 1 ; Current = ctx.Current + 1 }
            stringLiteral ctx'
        | _ ->
            let ctx' = { ctx with Current = ctx.Current + 1 }
            stringLiteral ctx'
    
let ignoreComment ctx =
    let src = ctx.Source.ToString()
    let eol = src.IndexOf('\n', ctx.Start)
    let length = src.Substring(ctx.Start, eol)
    { ctx with Start = eol + 1 ; Current = eol + 1 ; LineNum = ctx.LineNum + 1 }

let addTokenSimple ctx tokenType =
    let newToken = {
        TokenType = tokenType
        Lexeme = ctx.Source[ctx.Current] |> string
        Literal = None
        Line = ctx.LineNum
    }
    { ctx with Tokens = newToken::ctx.Tokens ; Start = ctx.Start + 1 ; Current = ctx.Current + 1 }

let addTokenWithMatch ctx expected yesMatch noMatch =
    let newToken = {
        TokenType = if checkNextChar ctx expected then yesMatch else noMatch
        Lexeme = ctx.Source[ctx.Current] |> string
        Literal = None
        Line = ctx.LineNum
    }
    { ctx with Tokens = newToken::ctx.Tokens ; Start = ctx.Start + 2 ; Current = ctx.Current + 2 }

let scanTokens (source : string) =
    let initCtx = {
        Source = source.ToCharArray()
        Start = 0
        Current = 0
        LineNum = 1
        Tokens = []
        FoundError = false
    }
    
    let rec scan ctx =
        match ctx |> advance |> peek with
        | Some '(' -> addTokenSimple ctx LeftParen
        | Some ')' -> addTokenSimple ctx RightParen
        | Some '{' -> addTokenSimple ctx RightBrace
        | Some '}' -> addTokenSimple ctx LeftBrace
        | Some ',' -> addTokenSimple ctx Comma
        | Some '.' -> addTokenSimple ctx Dot
        | Some '-' -> addTokenSimple ctx Minus
        | Some '+' -> addTokenSimple ctx Plus
        | Some ';' -> addTokenSimple ctx Semicolon
        | Some '*' -> addTokenSimple ctx Star
        | Some '!' -> addTokenWithMatch ctx '=' BangEqual    Bang
        | Some '=' -> addTokenWithMatch ctx '=' EqualEqual   Equal
        | Some '<' -> addTokenWithMatch ctx '=' LessEqual    Less
        | Some '>' -> addTokenWithMatch ctx '=' GreaterEqual Greater
        | Some '/' ->
            if checkNextChar ctx '/'
                then ignoreComment ctx
                else addTokenSimple ctx Slash
        | Some Digit ->
            numericLiteral ctx |> Result.ignoreErrorWith ctx
        | Some Digit ->
            numericLiteral ctx |> Result.ignoreErrorWith ctx
        | _ -> ctx
        |> scan
        
    scan initCtx

