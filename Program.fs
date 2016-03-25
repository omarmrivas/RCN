// Learn more about F# at http://fsharp.net
// See the 'F# Tutorial' project for more help.
//module RCN

open Microsoft.FSharp.Math
open Microsoft.FSharp.Collections
open System.Threading.Tasks

let initial_state best_so_far = 
    let vertices = [(0N, 0N)
                    (1N, 0N)
                    (1N/2N, 1N)]
    List.fold (fun g v -> match Graph.add_vertex best_so_far (Graph.P [], g, v, 0) with
                            Some (vertex, g) -> g
                          | None -> failwith "Impossible to raise")
              Graph.empty_graph vertices

let goal n best_so_far (g : Graph.PlanarGraph) =
    if List.length g.vertices = n && g.crossing_number <= best_so_far n
    then true
    else false

let succesor best_so_far (g : Graph.PlanarGraph) : (Graph.Vertex * Graph.PlanarGraph) list =
    let options = ParallelOptions()
    printfn "Configuring degree of parallelism: %A" options.MaxDegreeOfParallelism
    let center ((ux,uy),(vx,vy),(wx,wy)) = ((ux+vx+wx)/3N, (uy+vy+wy)/3N)
    let center' l = l |> List.fold (fun (cx,cy) (vx,vy) -> (cx+vx, cy+vy)) (0N,0N)
                      |> (fun (cx, cy) -> let n = l |> List.length
                                                    |> BigRational.FromInt
                                          (cx / n, cy / n))
    Graph.graph_to_gnuplot ("graph" + ((string << List.length) g.vertices)) g
    List.map (fun t -> (center t, Graph.T t)) g.triangles @ List.map (fun p -> (center' p, Graph.P p)) g.non_triangles
        |> Library.tap (fun arr -> printfn "Triangles & non-triangles: %A" (List.length arr))
        |> PSeq.withDegreeOfParallelism options.MaxDegreeOfParallelism
        |> PSeq.choose (Graph.crossing_number best_so_far g)
        |> PSeq.choose (Graph.add_vertex best_so_far)
        |> Library.tap (fun l -> printfn "Succesors with good crossing number: %A" (PSeq.length l))
        |> Library.tap (fun l -> l |> PSeq.map (fun (_, g) -> g.crossing_number)
                                   |> set
                                   |> printfn "Crossing numbers: %A")
        // Sort w.r.t crossing number and complexity of new vertex
        |> PSeq.map (fun (v, g) -> (v,g,(String.length << string) v))
        |> PSeq.sortBy (fun (_, g, i) -> (g.crossing_number, i))
        |> PSeq.map (fun (v, g, _) -> (v, g))
        |> PSeq.toList

[<EntryPoint>]
let main argv =
    let updated_crossings_data = OswinPage.crossings.Load("http://www.ist.tugraz.at/staff/aichholzer/research/rp/triangulations/crossing/")
    let crossings = OswinPage.min_crossings_so_far updated_crossings_data
    printfn "Input number: %A" argv.[0]
    let n = int argv.[0]
    printfn "Loading best graph with %A vertices..." n
    printfn "Best graph has %A crossing number" (crossings n)
    printfn "Starting DFS..."
    let stopWatch = System.Diagnostics.Stopwatch.StartNew()
    let r = Search.blp_rep (initial_state crossings) (goal n crossings) (succesor crossings) (uint32 n - 3u)
    stopWatch.Stop()
    printfn "%A" r
    printfn "%f" stopWatch.Elapsed.TotalMilliseconds
    0 // return an integer exit code

