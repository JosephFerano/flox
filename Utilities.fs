module Flox.Utilities

[<AutoOpen>]
module Prelude =
    let tap (fn : 'a -> unit) (a : 'a) =
        fn a
        a
    
[<RequireQualifiedAccess>]
module Option =
    let mapWithDefault f def = function
        | Some x -> f x
        | None -> def
        
[<AutoOpen>]
[<RequireQualifiedAccess>]
module Result =
    let (>>=) x f = Result.bind f x
    let (<!>) x f = Result.map f x
    let ignoreErrorWith value = function
        | Ok x -> x
        | Error _ -> value
    
    
module ErrorHandling =
    let report line where message =
        eprintfn $"[line {line}] Error: {where}: {message}"

    let error line message = report line "" message

