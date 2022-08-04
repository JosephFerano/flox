module Flox.Scanner

open Flox.Types
open Flox.Utilities

let reserved =
    [ "and" , And
      "class" , Class
      "else" , Else
      "false" , False
      "for" , For
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
    
type ScanErrorType =
    | UnterminatedString
    | UnexpectedCharacter
    
type ScanContext = {
    Source : string
    Start : int
    Current : int
    LineNum : int
    Tokens : Token list
    Errors : ScanErrorType list
}

let isAtEnd ctx = ctx.Current >= ctx.Source.Length
let advance ctx = { ctx with Current = ctx.Current + 1 }
let peek ctx = if isAtEnd ctx then None else Some ctx.Source[ctx.Current]
let peekNext ctx = if isAtEnd { ctx with Current = ctx.Current + 1 } then None else Some ctx.Source[ctx.Current + 1]
let matchNextChar ctx expected = not (isAtEnd ctx) && (ctx |> peekNext |> Option.mapWithDefault ((=) expected) false)

let isDigit c = c >= '0' && c <= '9'
let (|Digit|_|) c = if isDigit c then Some () else None
let isAlpha c = (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || c = '_'
let (|Alpha|_|) c = if isAlpha c then Some () else None
let isAlphaNumeric c = isAlpha c || isDigit c
let (|AlphaNumeric|_|) c = if isAlphaNumeric c then Some () else None
    
let newNumToken ctx =
    let num = ctx.Source[ctx.Start .. ctx.Current - 1] |> string
    { TokenType = Number
      Line = ctx.LineNum
      Literal = Some (float num)
      Lexeme = num }
    
let rec numericLiteral ctx =
    match peek ctx with
    | Some Digit -> ctx |> advance |> numericLiteral
    | Some '.' ->
        if peekNext ctx |> Option.mapWithDefault isDigit false then
            ctx |> advance |> numericLiteral
        else
            // Go back one character so the scanner can pick up the erroneous Dot
            { ctx with Tokens = (newNumToken ctx)::ctx.Tokens }
            |> advance
    | Some _
    | None -> { ctx with Tokens = (newNumToken ctx)::ctx.Tokens }

let rec stringLiteral ctx =
    match peekNext ctx with
    | Some '"' ->
        let token = { TokenType = TokenType.String
                      // Ignore starting quote
                      Lexeme = ctx.Source[ctx.Start + 1 .. ctx.Current] |> string
                      Literal = None
                      Line = ctx.LineNum }
        { ctx with Tokens = token::ctx.Tokens }
        |> advance
    | Some '\n' -> { ctx with LineNum = ctx.LineNum + 1 } |> advance |> stringLiteral 
    | Some _ -> ctx |> advance |> stringLiteral
    | None -> { ctx with Errors = UnterminatedString::ctx.Errors }
    
let rec identifier ctx =
    match peekNext ctx with
    | Some Alpha -> ctx |> advance |> identifier
    | Some AlphaNumeric -> ctx |> advance |> identifier
    | Some _
    | None ->
        let word = ctx.Source[ctx.Start .. ctx.Current] |> string
        let token = {
            TokenType =
                match reserved |> Map.tryFind word with
                | Some tt -> tt
                | None -> Identifier
            Lexeme = word
            Literal = None
            Line = ctx.LineNum
        }
        { ctx with Tokens = token::ctx.Tokens }
    
let ignoreComment ctx =
    let eol = ctx.Source.IndexOf('\n', ctx.Start)
    if eol >= 0
        then { ctx with Current = eol ; LineNum = ctx.LineNum + 1 }
        else { ctx with Current = ctx.Source.Length }

let addTokenSimple ctx tokenType =
    let newToken = {
        TokenType = tokenType
        Lexeme = ctx.Source[ctx.Current] |> string
        Literal = None
        Line = ctx.LineNum
    }
    { ctx with Tokens = newToken::ctx.Tokens }

let addTokenWithMatch ctx expected yesMatch noMatch =
    let tt , lexeme , curr = 
        match matchNextChar ctx expected with
        | true ->
            yesMatch , ctx.Source[ctx.Current .. ctx.Current + 1] , ctx.Current + 1
        | false ->
            noMatch , ctx.Source[ctx.Current] |> string , ctx.Current
    let newToken = { TokenType = tt ; Lexeme = lexeme ; Literal = None ; Line = ctx.LineNum }
    { ctx with Tokens = newToken::ctx.Tokens ; Current = curr }

let scanTokens (source : string) =
    let initCtx = {
        Source = source
        Start = 0
        Current = 0
        LineNum = 1
        Tokens = []
        Errors = []
    }
    
    let rec scan ctx =
        match ctx |> peek with
        | Some c ->
            match c with
            | '(' -> addTokenSimple ctx LeftParen
            | ')' -> addTokenSimple ctx RightParen
            | '{' -> addTokenSimple ctx LeftBrace
            | '}' -> addTokenSimple ctx RightBrace
            | ',' -> addTokenSimple ctx Comma
            | '.' -> addTokenSimple ctx Dot
            | '-' -> addTokenSimple ctx Minus
            | '+' -> addTokenSimple ctx Plus
            | ';' -> addTokenSimple ctx Semicolon
            | '*' -> addTokenSimple ctx Star
            | '!' -> addTokenWithMatch ctx '=' BangEqual    Bang
            | '=' -> addTokenWithMatch ctx '=' EqualEqual   Equal
            | '<' -> addTokenWithMatch ctx '=' LessEqual    Less
            | '>' -> addTokenWithMatch ctx '=' GreaterEqual Greater
            | '/' ->
                if matchNextChar ctx '/'
                    then ignoreComment ctx
                    else addTokenSimple ctx Slash
            | '\r' | ' ' | '\t' -> ctx // Ignore
            | '\n'  -> { ctx with LineNum = ctx.LineNum + 1 }
            | '"'   -> ctx |> stringLiteral
            | Digit -> ctx |> numericLiteral
            | Alpha -> ctx |> advance |> identifier
            | _ -> { ctx with Errors = UnexpectedCharacter::ctx.Errors }
            |> advance
            |> fun ctx -> { ctx with Start = ctx.Current }
            |> scan
        | None -> ctx
        
    let ctx = scan initCtx
    let eof = { TokenType = EOF ; Lexeme = "" ; Literal = None ; Line = ctx.LineNum }
    eof::ctx.Tokens |> List.rev , ctx.Errors |> List.rev
    

