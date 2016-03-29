module Graph

open System
open Microsoft.FSharp.Collections
open System.Net
open Microsoft.FSharp.Math
open System.Text.RegularExpressions
open FSharp.Collections.ParallelSeq
open Line
open Library
open System.IO

type Vertex = BigRational * BigRational
type Edge = Vertex * Vertex
type Triangle = Vertex * Vertex * Vertex
type Quadrilateral = Vertex * Vertex * Vertex * Vertex
type Polygon = Vertex list

exception INVALID_GRAPH of Vertex

[<Serializable>]
type PlanarGraph =
         {vertices: Vertex list
          edges : Edge list
          lines : line list
          wingA : Polygon list
          wingB : Polygon list
          wingC : Polygon list
          crossing_number: int
         }

let sort_points (l : Vertex list) =
    let (bx,by) = List.minBy snd l
    let (valid, invalid) = List.partition (fun (_, y) -> y <> by) l
    let valid = valid |> List.map (fun (x,y) -> (- (x - bx) / (y - by), (x,y)))
                      |> List.sortBy fst
                      |> List.map snd
    let (front, back) = List.partition (fun (x,y) -> x >= bx) invalid
    (front @ valid @ back)

let initial_graph = 
    let v1 = (0N, 0N)
    let v2 = (1N, 0N)
    let v3 = (1N/2N, 1N)
    let c = (1N/2N, 1N/3N)
    let bottom = Option.get (Line.crossing_point (Line.construct_line v1 v2, Line.construct_line v3 c))
    let t1 = sort_points [v1; c; bottom]
    let t2 = sort_points [v3; c; Option.get (Line.crossing_point (Line.construct_line v1 v3, Line.construct_line v2 c))]
    let t3 = sort_points [v2; c; bottom]
    let edges = 
        [0 .. (binomialCoefficient 4 2 - 1)]
                |> List.map (choose [c; v3; v2; v1] 2)
                |> List.map (function
                                | [v1; v2] -> (v1, v2)
                                | _ -> failwith "Impossible to raise this exception")
    {vertices = [c; v3; v2; v1]
     edges = edges
     lines = List.map (fun (u, v) -> Line.construct_line u v) edges
     wingA = [t1]
     wingB = [t3]
     wingC = [t2]
     crossing_number = 0
    }

let polygon_to_triangle = function
    | [u; v; w] -> (u, v, w)
    | _ -> failwith "Not a triangle"

let is_collinear ((x, y) : Vertex) ((x1, y1) : Vertex, (x2, y2) : Vertex) =
    let numerator = y2 - y1
    let denominator = x2 - x1
    if denominator = BigRational.Zero then
         x = x1
    else let m = numerator / denominator
         let y' = m * (x - x1) + y1
         y = y'

let cross_product2D ((vx, vy) : Vertex, (wx, wy) : Vertex) = vx * wy - vy * wx

let minus_vector2D ((vx, vy) : Vertex, (wx, wy) : Vertex) = (wx - vx, wy - vy) : Vertex

let edges_intersect (edge1 : Edge, edge2 : Edge) =
    let p = fst edge1
    let q = fst edge2
    let r = minus_vector2D edge1
    let s = minus_vector2D edge2
    let q_minus_p = minus_vector2D (p, q)
    let r_cross_s = cross_product2D (r, s)
    if r_cross_s = BigRational.Zero (* parallel edges *)
    then false
    else let t = (cross_product2D (q_minus_p, s)) / r_cross_s
         if t <= BigRational.Zero || BigRational.One <= t
         then false
         else let u = (cross_product2D (q_minus_p, r)) / r_cross_s
              if u <= BigRational.Zero || BigRational.One <= u
              then false
              else true

let intersect_quadrilateral ((v1, v2, v3, v4) : Quadrilateral) =
    List.exists edges_intersect [((v1, v2), (v3, v4)); ((v1, v3), (v2, v4)); ((v1, v4), (v2, v3))]

let point_in_triangle (px,py) ((p1x, p1y), (p2x, p2y), (p3x, p3y)) =
    let p23y = p2y - p3y
    let p03x = px - p3x
    let p32x = p3x - p2x
    let p03y = py - p3y
    let p13x = p1x - p3x
    let p31y = p3y - p1y
    let p23yp13x = p23y*p13x
    let p32xp31y = p32x*p31y
    let alpha = (p23y*p03x + p32x*p03y) /
                (p23yp13x - p32xp31y)
    let beta = (p31y*p03x + p13x*p03y) /
               (p23yp13x - p32xp31y)
    let gamma = 1N - alpha - beta
    alpha >= 0N && beta >= 0N && gamma >= 0N

let convex_hull = ((0N, 0N),(1N, 0N),(1N/2N, 1N))

let center ((ux,uy),(vx,vy),(wx,wy)) = ((ux+vx+wx)/3N, (uy+vy+wy)/3N)

let center' l = l |> List.fold (fun (cx,cy) (vx,vy) -> (cx+vx, cy+vy)) (0N,0N)
                  |> (fun (cx, cy) -> let n = l |> List.length
                                                |> BigRational.FromInt
                                      (cx / n, cy / n))

let polygon_crossed sc lines (vs : Polygon) : Polygon list =
    let inside (u, v) p = 
        let uv = v2d_sub u v
        let d = v2d_dot uv uv
        if d = 0N
        then printfn "Invalid polygon! %A" vs
             false
        else let alpha = (v2d_dot (v2d_sub p v) uv) / d
             alpha > 0N && alpha < 1N
    let create (u, v) line =
        match crossing_point (line, construct_line u v) with
            | Some uv -> if inside (u, v) uv
                         then Some (line, uv)
                         else None
            | None -> None
//    let vs = sort_points vs
    vs  |> List.pairwise
        |> (fun l -> (List.head vs, List.last vs) :: l)
        |> List.collect (fun p -> List.choose (create p) lines)
        |> (fun l -> if List.isEmpty l
                     then [vs]
                     else let vs = List.map snd l @ vs
                                     |> (Set.toList << set)
                          let vs = match sc with
                                    | Some c -> c :: vs
                                    | None -> vs
                          let lines = List.map fst l
                          let size = List.length vs
                          let n = Library.binomialCoefficient size 3 - 1
                          let triangles = 
                            [0 .. n]
                                |> List.map (List.sort << (Library.choose vs 3))
                                |> List.map (fun t -> match t with
                                                        | [u;v;w] -> (u,v,w)
                                                        | _ -> failwith "Impossible to raise")
                                |> List.filter (fun (u,v,w) -> not (is_collinear u (v,w)) &&
                                                               not (List.exists (line_crosses_triangle (u,v,w)) lines))
                          let codes = triangles
                                        |> List.map (fun t -> let c = center t
                                                              (List.map (Line.find_side c) lines, t))
                                        |> List.fold (fun codes (code,t) -> 
                                            match Map.tryFind code codes with
                                                | Some triangles -> Map.add code (t :: triangles) codes
                                                | None -> Map.add code [t] codes) Map.empty
                          codes |> Map.toList
                                |> List.fold (fun polygons (_, trs) ->
                                    let nt = trs |> List.map (fun (u,v,w) -> [u;v;w])
                                                 |> List.concat
                                                 |> (sort_points << Set.toList << set)
                                    nt :: polygons) [])

let parallel_calculate_polygons parallelism wingA wingB wingC lines vertices poly vertex =
    let new_lines =  List.map (Line.construct_line vertex) vertices
    let (wingA, fooA) = if List.exists (fun p' -> poly = p') wingA
                        then (List.filter (fun p' -> poly <> p') wingA, true)
                        else (wingA, false)
    let (wingB, fooB) = if not fooA && List.exists (fun p' -> poly = p') wingB
                        then (List.filter (fun p' -> poly <> p') wingB, true)
                        else (wingB, false)
    let (wingC, fooC) = if not fooA && not fooB && List.exists (fun p' -> poly = p') wingC
                        then (List.filter (fun p' -> poly <> p') wingC, true)
                        else (wingC, false)
    let wingA =
        wingA |> PSeq.withDegreeOfParallelism parallelism
              |> PSeq.map (polygon_crossed None new_lines)
              |> PSeq.concat
              |> PSeq.toList
    let wingB =
        wingB |> PSeq.withDegreeOfParallelism parallelism
              |> PSeq.map (polygon_crossed None new_lines)
              |> PSeq.concat
              |> PSeq.toList
    let wingC =
        wingC |> PSeq.withDegreeOfParallelism parallelism
              |> PSeq.map (polygon_crossed None new_lines)
              |> PSeq.concat
              |> PSeq.toList
    let wing = polygon_crossed (Some vertex) new_lines poly
    let (wingA, wingB, wingC) =
        match (fooA, fooB, fooC) with
            | (true, _, _) -> (wing @ wingA, wingB, wingC)
            | (_, true, _) -> (wingA, wing @ wingB, wingC)
            | (_, _, true) -> (wingA, wingB, wing @ wingC)
            | _ -> failwith "Impossible to raise"
    (wingA, wingB, wingC, new_lines @ lines)

let calculate_polygons wingA wingB wingC lines vertices poly vertex =
    let new_lines =  List.map (Line.construct_line vertex) vertices
    let (wingA, fooA) = if List.exists (fun p' -> poly = p') wingA
                        then (List.filter (fun p' -> poly <> p') wingA, true)
                        else (wingA, false)
    let (wingB, fooB) = if not fooA && List.exists (fun p' -> poly = p') wingB
                        then (List.filter (fun p' -> poly <> p') wingB, true)
                        else (wingB, false)
    let (wingC, fooC) = if not fooA && not fooB && List.exists (fun p' -> poly = p') wingC
                        then (List.filter (fun p' -> poly <> p') wingC, true)
                        else (wingC, false)
    let wingA =
        wingA |> List.map (polygon_crossed None new_lines)
              |> List.concat
    let wingB =
        wingB |> List.map (polygon_crossed None new_lines)
              |> List.concat
    let wingC =
        wingC |> List.map (polygon_crossed None new_lines)
              |> List.concat
    let wing = polygon_crossed (Some vertex) new_lines poly
    let (wingA, wingB, wingC) =
        match (fooA, fooB, fooC) with
            | (true, _, _) -> (wing @ wingA, wingB, wingC)
            | (_, true, _) -> (wingA, wing @ wingB, wingC)
            | (_, _, true) -> (wingA, wingB, wing @ wingC)
            | _ -> failwith "Impossible to raise"
    (wingA, wingB, wingC, new_lines @ lines)

let crossing_number best_so_far g (vertex, poly) =
    let size = List.length g.vertices
    let cn = [0 .. (binomialCoefficient size 3 - 1)]
                |> List.map ((fun three -> List.sort (vertex :: three)) << (choose g.vertices 3))
                |> List.map (function | [v1; v2; v3; v4] -> (v1, v2, v3, v4)
                                      | _ -> failwith "Impossible to raise this exception")
                |> List.sumBy (fun quad -> if intersect_quadrilateral quad
                                           then 1
                                           else 0)
    if g.crossing_number + cn > best_so_far (size + 1)
    then None
    else Some (poly, g, vertex, cn)

let add_vertex parallelism best_so_far (poly, g, vertex, cn) =
    let n = List.length g.vertices
    if List.exists (is_collinear vertex) g.edges then None
    // base case
    else if n <= 3
    then None
    // non-base case
    else let (wingA, wingB, wingC, lines) =
            match parallelism with
                | Some parallelism -> parallel_calculate_polygons parallelism g.wingA g.wingB g.wingC g.lines g.vertices poly vertex
                | None -> calculate_polygons g.wingA g.wingB g.wingC g.lines g.vertices poly vertex
         {vertices = vertex :: g.vertices;
          edges = List.map (fun v -> (vertex,v)) g.vertices @ g.edges;
          wingA = wingA; wingB = wingB; wingC = wingC; lines = lines;
          crossing_number = g.crossing_number + cn}
            |> (fun g -> Some (poly, vertex, g))
