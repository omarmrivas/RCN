module Search

open System

let blp_rep inicio meta sucesores m =
  match Stack.get_best (m + 3u) with
  | Some sol -> Some sol
  | None -> 
    if Stack.empty ()
    then Stack.push (0u, inicio, [])
    else ()
    let rec dfs_aux () =
        match Stack.peak () with
            | Some (id, h, state, sol) -> 
                if meta state then Some (List.rev sol)
                else if h < m
                     then state |> sucesores
                                |> List.map (fun (a, st) -> (h + 1u, st, a :: sol))
                                |> Library.tap (fun ns -> printfn "Changing DB..."
                                                          if List.isEmpty ns
                                                          then ()
                                                          else Stack.update_best (List.head ns))
                                |> List.rev
                                |> (fun ns -> Stack.delete id
                                              List.iter (fun n -> Stack.push n) ns
                                              printfn "Done...")
                                |> dfs_aux
                     else dfs_aux ()
            | None -> None
    dfs_aux ()
