// Learn more about F# at http://fsharp.net
// See the 'F# Tutorial' project for more help.
//module RCN

open Microsoft.FSharp.Math
//open Microsoft.FSharp.Collections
open System.Threading.Tasks
open FSharp.Collections.ParallelSeq

let initial_state = 
    Graph.initial_graph

let goal n best_so_far (g : Graph.PlanarGraph) =
    if List.length g.vertices = n && g.crossing_number <= best_so_far n
    then true
    else false

// Select only one wing
let select_wing (g : Graph.PlanarGraph) =
    let n = List.length g.vertices
    if n <= 3
    then []
    else let wing = (n-4) % 3
         match wing with
            | 0 -> g.wingA
            | 1 -> g.wingB
            | _ -> g.wingC
    
let succesor parallelism best_so_far (g : Graph.PlanarGraph) : (Graph.Vertex * Graph.PlanarGraph) list =
    let center l = l |> List.fold (fun (cx,cy) (vx,vy) -> (cx+vx, cy+vy)) (0N,0N)
                     |> (fun (cx, cy) -> let n = l |> List.length
                                                   |> BigRational.FromInt
                                         (cx / n, cy / n))
    g |> select_wing
      |> Library.tap (fun arr -> printfn "Triangles & non-triangles: %A" (List.length arr))
(*      |> List.map (fun t -> (center t, t))
      |> List.choose (Graph.crossing_number best_so_far g)
      |> List.choose (Graph.add_vertex best_so_far)*)
      |> PSeq.withDegreeOfParallelism parallelism
      |> PSeq.map (fun t -> (center t, t))
      |> PSeq.choose (Graph.crossing_number best_so_far g)
      |> PSeq.toList
      |> List.choose (Graph.add_vertex parallelism best_so_far)
      |> Library.tap (fun l -> printfn "Succesors with good crossing number: %A" (List.length l))
      |> Library.tap (fun l -> l |> List.map (fun (_, _, g) -> g.crossing_number)
                                 |> set
                                 |> printfn "Crossing numbers: %A")
      // Sort w.r.t crossing number and complexity of new vertex
      |> List.map (fun (poly, v, g) -> (poly,v,g,(String.length << string) v))
      |> List.sortBy (fun (_, _, g, i) -> (g.crossing_number, i))
      |> List.map (fun (poly, v, g, _) -> (poly, v, g))
      |> Library.tap (fun l -> let polygons = List.map (fun (poly, _, _) -> poly) l
                               printfn "Changing DB..."
                               Stack.update_best polygons (g, g.vertices)
                               if List.isEmpty l
                               then ()
                               else let (_, _, g) = List.head l
                                    Stack.update_best ([] : Graph.Vertex list list) (g, g.vertices))
      |> List.map (fun (_, v, g) -> (v, g))

[<EntryPoint>]
let main argv =
    if argv.[0] = "-find"
    then let updated_crossings_data = OswinPage.crossings.Load("http://www.ist.tugraz.at/staff/aichholzer/research/rp/triangulations/crossing/")
         let crossings = OswinPage.min_crossings_so_far updated_crossings_data
         printfn "Number of vertices: %A" argv.[1]
         printfn "Degree of parallelism: %A" argv.[2]
         let n = int argv.[1]
         let parallelism = int argv.[2]
         printfn "Loading best graph with %A vertices..." n
         printfn "Best graph has %A crossing number" (crossings n)
         printfn "Starting DFS..."
         let stopWatch = System.Diagnostics.Stopwatch.StartNew()
         let r = Search.blp_rep initial_state (goal n crossings) (succesor parallelism crossings) (uint32 n - 3u)
         stopWatch.Stop()
         printfn "%A" r
         printfn "%f" stopWatch.Elapsed.TotalMilliseconds
    else if argv.[0] = "-print"
    then let n = int argv.[1]
         let debug = argv.[2] = "true"
         Gnuplot.graph_to_gnuplot' debug ("graph" + string n) (uint32 n)
    else if argv.[0] = "-animation"
    then let n = int argv.[1]
         Gnuplot.graph_to_animation "graph" n
    else ()
    0 // return an integer exit code

