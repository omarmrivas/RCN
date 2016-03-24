module Library

open Microsoft.FSharp.Math
open System.IO
open Nessos.FsPickler

(*
    Elementary basic functions
*)
let tap f = fun x -> (f x; x)

let swap (x, y) = (y, x)

let pairself f (x, y) = (f x, f y)


(*
    Numeric functions 
*)
let square_root epsilon S =
    let epsilon = BigRational.Abs epsilon
    let half = 1N / 2N
    let quarter = 1N / 4N
    let three = 3N
    let rec square_aux a c =
            if BigRational.Abs (S - (a * a)) < epsilon
            then a
            else square_aux (a - (a * c * half)) (c * c * (c - three) * quarter)
    square_aux S (S - BigRational.One)


(*
    Combinatoric functions
*)
let next_digit Ln L =
    (List.zip L Ln)
    |> List.rev
    |> List.fold (fun (foo, L) (i, i_n) ->
                         if foo then 
                             if i + 1 < i_n 
                             then (false, (i + 1) :: L)
                             else (true, 0 :: L)
                         else (foo, i :: L)) (true, []) 
    |> (fun (foo, L) -> if foo then None
                        else Some L)

let lazy_one_of_each LL =
    let sizes = List.map List.length LL
    let foo = List.exists (fun n -> n = 0) sizes
    let state = [for i in 1 .. List.length LL - 1 -> 0]// (1 upto (length LL - 1))
                      |> (fun l -> l @ [-1])
    let rec one_of_each state = 
        seq {match next_digit sizes state with
                        Some state -> yield (List.map2 (fun x y -> List.item y x) LL state)
                                      yield! one_of_each state
                      | None -> ()}
    if foo then Seq.empty
    else one_of_each state

let binomialCoefficient n k =
    if k < 0 || k > n
    then 0
    else
      let k = if k > n - k 
              then n - k
              else k
      let n_k = BigRational.FromInt(n - k)
      let c = BigRational.One
      [1N .. BigRational.FromInt k]
            |> List.fold (fun c i -> c * (n_k + i) / i) c
            |> BigRational.ToInt32

let choose set k x =
    let rec maximize a b x =
                if (binomialCoefficient a b) <= x then a
                else maximize (a - 1) b x
    let rec iterate n x i = 
                match i with
                | 0 -> []
                | i -> let max = maximize n i x
                       max :: iterate n (x - (binomialCoefficient max i)) (i - 1)
    if x < 0 then failwith "x < 0 !!!"
    else let idxs = iterate (List.length set) x k
         List.sort (List.map (fun indx -> List.item indx set) (List.sort idxs))


(*
    Functions on lists
*)
let butlast l =
    let rec butlast_tail acc =
        function x :: [] -> List.rev acc
               | x :: xs -> butlast_tail (x :: acc) xs
               | [] -> failwith "No last element"
    butlast_tail [] l

(*separate s [x1, x2, ..., xn]  ===>  [x1, s, x2, s, ..., s, xn]*)
let rec separate s L =
    match L with
        | x :: (_ :: _ as xs) -> x :: s :: separate s xs
        | xs -> xs

let serialize obj =
    let binarySerializer = FsPickler.CreateBinarySerializer()
    binarySerializer.Pickle obj

let deserialize<'T> (bytes : byte[]) =
    let binarySerializer = FsPickler.CreateBinarySerializer()
    binarySerializer.UnPickle<'T> bytes

