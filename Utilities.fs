module Flox.Utilities

[<RequireQualifiedAccess>]
module Option =
    let mapWithDefault f def = function
        | Some x -> f x
        | None -> def
        
[<RequireQualifiedAccess>]
module Result =
    let ignoreErrorWith value = function
        | Ok x -> x
        | Error _ -> value
        
module ErrorHandling =
    let report line where message =
        eprintfn $"[line {line}] Error: {where}: {message}"

    let error line message = report line "" message

