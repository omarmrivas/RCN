module Parallel

open System
open System.Collections.Concurrent
open System.Linq
open System.Threading.Tasks
open Microsoft.FSharp.Collections

let map parallelism (f: 'T -> 'U) (array : 'T[]) : 'U[]=
    if Array.isEmpty array
    then [||]
    else let inputLength = array.Length
         let result = Array.zeroCreate inputLength
         let options = ParallelOptions()
         options.MaxDegreeOfParallelism <- parallelism
         Parallel.For(0, inputLength, options, fun i ->
            result.[i] <- f array.[i]) |> ignore
         result


let choose parallelism f (array: 'T[]) =
    if Array.isEmpty array
    then [||]
    else let inputLength = array.Length
         let lastInputIndex = inputLength - 1

         let isChosen : bool [] = Array.zeroCreate inputLength
         let results : 'U [] = Array.zeroCreate inputLength
                
         let options = ParallelOptions()
         options.MaxDegreeOfParallelism <- parallelism

         Parallel.For(0, inputLength, options, (fun i -> 
            match f array.[i] with 
            | None -> () 
            | Some v -> 
                isChosen.[i] <- true; 
                results.[i] <- v
                )) |> ignore
                                                                                      
         let mutable outputLength = 0                
         for i = 0 to lastInputIndex do 
            if isChosen.[i] 
            then outputLength <- outputLength + 1
                        
         let output = Array.zeroCreate outputLength
         let mutable curr = 0
         for i = 0 to lastInputIndex do 
            if isChosen.[i] 
            then output.[curr] <- results.[i]
                 curr <- curr + 1
         output
