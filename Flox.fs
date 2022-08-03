module Flox.Main

open System
open System.IO
open Flox

let run source =
    let tokens = Scanner.scanTokens source
    for t in tokens do
        printfn $"Token: \"{t}\""

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