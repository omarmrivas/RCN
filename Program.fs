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
    List.fold (fun g v -> match Graph.add_vertex best_so_far ([], g, v, 0) with
                            Some (_, vertex, g) -> g
                          | None -> failwith "Impossible to raise")
              Graph.empty_graph vertices

let goal n best_so_far (g : Graph.PlanarGraph) =
    if List.length g.vertices = n && g.crossing_number <= best_so_far n
    then true
    else false

let v1 = (0N, 0N)
let v2 = (1N, 0N)
let v3 = (1N/2N, 1N)
let c = (1N/2N, 1N/3N)
let bottom = Option.get (Line.crossing_point (Line.construct_line v1 v2, Line.construct_line v3 c))
let t1 = (v1, c, bottom)
let t2 = (v3, c, Option.get (Line.crossing_point (Line.construct_line v1 v3, Line.construct_line v2 c)))
let t3 = (v2, c, bottom)
let triangles = [|t1; t3; t2|]

// Select only one wing
let select_wing (g : Graph.PlanarGraph) (c, _) =
    let n = List.length g.vertices
    if n <= 3
    then true
    else Graph.point_in_triangle c triangles.[(n-4) % 3]
    
let succesor parallelism best_so_far (g : Graph.PlanarGraph) : (Graph.Vertex * Graph.PlanarGraph) list =
    let center l = l |> List.fold (fun (cx,cy) (vx,vy) -> (cx+vx, cy+vy)) (0N,0N)
                     |> (fun (cx, cy) -> let n = l |> List.length
                                                   |> BigRational.FromInt
                                         (cx / n, cy / n))
    List.map (fun t -> (center t, t)) g.polygons
        |> Library.tap (fun arr -> printfn "Triangles & non-triangles: %A" (List.length arr))
        |> PSeq.withDegreeOfParallelism parallelism
        |> PSeq.filter (select_wing g)
        |> PSeq.choose (Graph.crossing_number best_so_far g)
        |> PSeq.choose (Graph.add_vertex best_so_far)
        |> PSeq.toList
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
         let r = Search.blp_rep (initial_state crossings) (goal n crossings) (succesor parallelism crossings) (uint32 n - 3u)
         stopWatch.Stop()
         printfn "%A" r
         printfn "%f" stopWatch.Elapsed.TotalMilliseconds
    else if argv.[0] = "-print"
    then let n = int argv.[1]
         Gnuplot.graph_to_gnuplot' false ("graph" + string n) (uint32 n)
    else if argv.[0] = "-animation"
    then let n = int argv.[1]
         Gnuplot.graph_to_animation "graph" n
    else ()
    0 // return an integer exit code

