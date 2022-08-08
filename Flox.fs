module Flox.Main

open System
open System.IO
open Flox
open Flox.Types
open Flox.Utilities

let run source =
    printfn "Tokens:"
    let tokens , errors = Scanner.scanTokens source
    List.iter (fun t -> printfn $"\t{t.TokenType } {t.Lexeme}") tokens
    if errors.Length > 0 then
        printfn "\tErrors:"
        errors |> List.iter  (fun t -> printfn $"\t\t{t}")
        errors
        |> List.map string
        |> String.concat "\n"
        |> Error
    else
        Parser.parseTokens tokens
        |> tap (Parser.prettyPrint >> printfn "%s")
        |> Interpreter.interpret
        |> Result.mapError (fun e -> e.Message + $"\n[line {e.Token.Line}]")
        
let runFile filename =
    let script = File.ReadAllText(filename)
    match run script with
    | Ok prim -> Interpreter.printPrimitive prim |> printfn "%A"
    | Error e ->
        eprintfn $"{e}"
        exit 70
    
let runPrompt () =
    let mutable running = true
    while running do
        printf "> "
        let line = Console.ReadLine()
        if line = null then
            running <- false
        else
            match run line with
            | Ok prim -> Interpreter.printPrimitive prim |> printfn "%A"
            | Error e -> eprintfn $"{e}"
    

match Environment.GetCommandLineArgs() with
| a when a.Length > 2 ->
    printfn "Usage: flox [script]"
    exit 64
| a when a.Length = 2 ->
    runFile a[1]
| _ ->
    runPrompt ()