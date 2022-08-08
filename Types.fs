module Flox.Types

type TokenType =
    // Single char
    | LeftParen
    | RightParen
    | LeftBrace
    | RightBrace
    | Comma
    | Dot
    | Minus
    | Plus
    | Semicolon
    | Slash
    | Star
    // One or Two char tokens
    | Bang
    | BangEqual
    | Equal
    | EqualEqual
    | Greater
    | GreaterEqual
    | Less
    | LessEqual
    // Literals
    | Identifier
    | String
    | Number
    // Keywords
    | And
    | Class
    | Else
    | Fun
    | For
    | If
    | Nil
    | Or
    | Print
    | Return
    | Super
    | This
    | True
    | False
    | Var
    | While
    | EOF
    

type Primitive =
    | Null
    | Num of float
    | Str of string
    | Bool of bool

[<RequireQualifiedAccess>]
module Primitive =
    let getTypeName = function
        | Null   -> "nil"
        | Num  _ -> "num"
        | Str  _ -> "string"
        | Bool _ -> "bool"
    
type Token = {
    TokenType : TokenType
    Lexeme : string
    Line : int
}

type ScanErrorType =
    | UnterminatedString
    | UnexpectedCharacter
    










