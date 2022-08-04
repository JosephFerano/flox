module Flox.Main

open System
open System.IO
open Flox
open Flox.Types

let run source =
    printfn "Tokens:"
    let tokens , errors = Scanner.scanTokens source
    List.iter (fun t -> printfn $"\t\"{t.TokenType } ({t.Lexeme})\"") tokens
    if errors.Length > 0 then
        printfn "\tErrors:"
        errors |> List.iter  (fun t -> printfn $"\t\t{t}")
        

let runFile filename =
    let script = File.ReadAllText(filename)
    run script
    
let runPrompt () =
    let mutable running = true
    while running do
        printf "> "
        let line = Console.ReadLine()
        if line = null then
            running <- false
        else
            run line
    

match Environment.GetCommandLineArgs() with
| a when a.Length > 2 ->
    printfn "Usage: flox [script]"
    exit 64
| a when a.Length = 2 ->
    runFile a[1]
| _ ->
    runPrompt ()