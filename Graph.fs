module Graph

open System
open Microsoft.FSharp.Collections
open System.Net
open Microsoft.FSharp.Math
open System.Text.RegularExpressions
open Line
open Library
open System.IO

type Vertex = BigRational * BigRational
type Edge = Vertex * Vertex
type Triangle = Vertex * Vertex * Vertex
type Quadrilateral = Vertex * Vertex * Vertex * Vertex

exception INVALID_GRAPH of Vertex

[<Serializable>]
type PlanarGraph =
         {vertices: Vertex list
          edges : Edge list
          lines : line list
          triangles : Triangle list
          non_triangles : Vertex list list
          quadrilaterals: Quadrilateral list
          crossing_number: int
         }

type polygon = T of Triangle
             | P of Vertex list

let empty_graph = {vertices = []
                   edges = []
                   lines = []
                   triangles = []
                   non_triangles = []
                   quadrilaterals = []
                   crossing_number = 0}

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

let sort_points (l : Vertex list) =
    let (bx,by) = List.minBy snd l
    let (valid, invalid) = List.partition (fun (_, y) -> y <> by) l
    let valid = valid |> List.map (fun (x,y) -> (- (x - bx) / (y - by), (x,y)))
                      |> List.sortBy fst
                      |> List.map snd
    let (front, back) = List.partition (fun (x,y) -> x >= bx) invalid
    (front @ valid @ back)

(*let less (centerx, centery) (ax, ay) (bx, by) =
    if ax - centerx >= 0N && bx - centerx < 0N
    then true
    else if ax - centerx < 0N && bx - centerx >= 0N
    then false
    else if ax - centerx = 0N && bx - centerx = 0N
    then if ay - centery >= 0N || by - centery >= 0N
         then ay > by
         else by > ay
    else let det = (ax - centerx) * (by - centery) - (bx - centerx) * (ay - centery)
         if det < 0N
         then true
         else if det > 0N
         then false
         else let d1 = (ax - centerx) * (ax - centerx) + (ay - centery) * (ay - centery)
              let d2 = (bx - centerx) * (bx - centerx) + (by - centery) * (by - centery)
              d1 > d2

let less_i c x y =
    if less c x y
    then 1
    else -1*)

let triangle_crossed (u,v,w) sc lines =
    let inside (u, v) p = 
        let uv = v2d_sub u v
        let d = v2d_dot uv uv
        if d = 0N
        then printfn "Invalid triangle! %A" (u,v,w)
             false
        else let alpha = (v2d_dot (v2d_sub p v) uv) / d
             alpha > 0N && alpha < 1N
    let create (u, v) line =
        match crossing_point (line, construct_line u v) with
            | Some uv -> if inside (u, v) uv
                         then Some (line, uv)
                         else None
            | None -> None
(*    let sort_polygon l =
        let c = center' l
        List.sortWith (less_i c) l*)
    [(u, v); (u, w); (v, w)]
        |> List.collect (fun p -> List.choose (create p) lines)
        |> (fun l -> if List.isEmpty l
                     then ([(u,v,w)], [])
                     else let vs = List.map snd l @ [u; v; w]
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
                                |> List.toArray
                          let codes = triangles
                                        |> Array.Parallel.map (fun t -> let c = center t
                                                                        (List.map (Line.find_side c) lines, t))
                                        |> Array.fold (fun codes (code,t) -> 
                                            match Map.tryFind code codes with
                                                | Some triangles -> Map.add code (t :: triangles) codes
                                                | None -> Map.add code [t] codes) Map.empty
                          codes |> Map.toList
                                |> List.fold (fun (triangles, non_triangles) (_, trs) ->
                                    if List.length trs > 1
                                    then let nt = trs |> List.map (fun (u,v,w) -> [u;v;w])
                                                      |> List.concat
                                                      |> (sort_points << Set.toList << set)
                                         (triangles, nt :: non_triangles)
                                    else (List.head trs :: triangles, non_triangles)) ([], []))

let polygon_crossed (vs : Vertex list) sc lines =
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
    let c = match sc with
                | Some c -> c
                | None -> center' vs
(*    let sort_polygon c l =
        List.sortWith (less_i c) l
    let sort_polygon' l =
        let c = center' l
        sort_polygon c l*)
//    vs  |> sort_polygon c
    vs  |> sort_points
        |> List.pairwise
        |> (fun l -> (List.head vs, List.last vs) :: l)
        |> List.collect (fun p -> List.choose (create p) lines)
        |> (fun l -> if List.isEmpty l
                     then ([], [vs])
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
                                |> List.toArray
                          let codes = triangles
                                        |> Array.Parallel.map (fun t -> let c = center t
                                                                        (List.map (Line.find_side c) lines, t))
                                        |> Array.fold (fun codes (code,t) -> 
                                            match Map.tryFind code codes with
                                                | Some triangles -> Map.add code (t :: triangles) codes
                                                | None -> Map.add code [t] codes) Map.empty
                          codes |> Map.toList
                                |> List.fold (fun (triangles, non_triangles) (_, trs) ->
                                    if List.length trs > 1
                                    then let nt = trs |> List.map (fun (u,v,w) -> [u;v;w])
                                                      |> List.concat
                                                      |> (sort_points << Set.toList << set)
                                         (triangles, nt :: non_triangles)
                                    else (List.head trs :: triangles, non_triangles)) ([], []))

let calculate_triangles triangles polygons lines vertices poly vertex =
    let new_lines = vertices |> List.toArray
                             |> Array.Parallel.map (Line.construct_line vertex)
    let new_lines' = Array.toList new_lines
    let triangles = match poly with
                        | T t -> List.filter (fun t' -> t <> t') triangles
                        | _ -> triangles
    let polygons = match poly with
                    | P p -> List.filter (fun p' -> p <> p') polygons
                    | _ -> polygons
    let (triangles', polygons') = 
        triangles |> List.toArray
                  |> Array.Parallel.map (fun t -> triangle_crossed t None new_lines')
                  |> Array.fold (fun (triangles, polygons) (ts, ps) -> (ts @ triangles, ps @ polygons)) ([], [])
    let (triangles'', polygons'') = 
        polygons |> List.toArray
//                 |> Array.map (fun t -> polygon_crossed t None new_lines')
                 |> Array.Parallel.map (fun t -> polygon_crossed t None new_lines')
                 |> Array.fold (fun (triangles, polygons) (ts, ps) -> (ts @ triangles, ps @ polygons)) ([], [])
    let (triangles''', polygons''') =
        match poly with
            | T t -> triangle_crossed t (Some vertex) new_lines'
            | P p -> polygon_crossed p (Some vertex) new_lines'
    (triangles' @ triangles'' @ triangles''', polygons' @ polygons'' @ polygons''', new_lines' @ lines)


(*let calculate_triangles triangles lines vertices v =
    let new_lines = vertices |> List.toArray
                             |> Array.Parallel.map (Line.construct_line v)
    printfn "triangles"
    let triangles =
        triangles |> List.toArray
                  |> Array.Parallel.collect (fun t -> Array.collect (triangle_crossed t) new_lines)
                  |> Array.Parallel.choose (fun t -> if Array.exists (Line.line_crosses_triangle t) new_lines
                                                     then None
                                                     else Some t)
    printfn "codes"
    let all_lines = new_lines |> Array.toList
                              |> List.append lines
    let codes = triangles
                    |> Array.Parallel.map (fun t -> let c = center t
                                                    (List.map (Line.find_side c) all_lines, t))
                    |> Array.fold (fun codes (code,t) -> 
                                        match Map.tryFind code codes with
                                            | Some triangles -> Map.add code (t :: triangles) codes
                                            | None -> Map.add code [t] codes) Map.empty
    let (triangles, non_triangles) =
        codes |> Map.toList
              |> List.fold (fun (triangles, non_triangles) (_, trs) ->
                                if List.length trs > 1
                                then let nt = trs |> List.map (fun (u,v,w) -> [u;v;w])
                                                  |> List.concat
                                                  |> (Set.toList << set)
                                     (triangles, nt :: non_triangles)
                                else (List.head trs :: triangles, non_triangles)) ([], [])
    (triangles, non_triangles, all_lines)*)

let add_vertex best_so_far
               {vertices = vertices;
                edges = edges; 
                lines = lines;
                triangles = triangles;
                non_triangles = non_triangles;
                quadrilaterals = quadrilaterals;
                crossing_number = crossing_number} poly vertex =
    //printfn "%A" vertex
    if List.exists (is_collinear vertex) edges then None
    else if List.length vertices < 3 then
      let vertices = vertex :: vertices
      let size = List.length vertices
      let edges = [0 .. (binomialCoefficient size 2 - 1)]
                    |> List.map (choose vertices 2)
                    |> List.map (function
                                  | [v1; v2] -> (v1, v2)
                                  | _ -> failwith "Impossible to raise this exception")
      let triangles =
                [0 .. (binomialCoefficient size 3 - 1)]
                    |> List.map (List.sort << choose vertices 3)
                    |> List.map (function
                                  | [v1; v2; v3] -> (v1, v2, v3)
                                  | _ -> failwith "Impossible to raise this exception")
      {vertices = vertices;
       edges = edges;
       lines = List.map (fun (u, v) -> Line.construct_line u v) edges;
       triangles = triangles;
       non_triangles = non_triangles;
       quadrilaterals = quadrilaterals;
       crossing_number = crossing_number}
        |> Some
    else
      let size = List.length vertices
      let vertices' = vertex :: vertices
      let new_quadrilaterals = [0 .. (binomialCoefficient size 3 - 1)]
                                |> List.map ((fun three -> List.sort (vertex :: three)) << (choose vertices 3))
                                |> List.map (function
                                                | [v1; v2; v3; v4] -> (v1, v2, v3, v4)
                                                | _ -> failwith "Impossible to raise this exception")
      let crossing_number' = new_quadrilaterals |> List.toArray
                                                |> Array.Parallel.map
                                                    (fun quad -> if intersect_quadrilateral quad
                                                                 then 1
                                                                 else 0)
                                                |> Array.sum
      if crossing_number + crossing_number' > best_so_far (size + 1)
      then None
      else
        let (triangles, non_triangles, lines) = calculate_triangles triangles non_triangles lines vertices poly vertex
        //printfn "crossing_number'"
        {vertices = vertices'; edges = List.map (fun v' -> (vertex,v')) vertices @ edges; 
        triangles = triangles; lines = lines;
        quadrilaterals = quadrilaterals @ new_quadrilaterals; non_triangles = non_triangles;
        crossing_number = crossing_number + crossing_number'}
            |> Some

let graph_to_gnuplot file (g : PlanarGraph) =
    let vertex_set = set g.vertices
    let all_vertices = 
        g.triangles
            |> List.map (fun (u,v,w) -> [u;v;w])
            |> List.append g.non_triangles
            |> List.concat
            |> (Set.toList << set)
            |> List.partition (fun v -> Set.contains v vertex_set)
            |> (fun (x,y) -> x @ y)
    let mvertex = all_vertices
                    |> List.mapi (fun i x -> (i+1, x))
                    |> List.fold (fun m (i,v) -> Map.add v i m) Map.empty
    let aux1 v = if Set.contains v vertex_set
                 then "V" + (string << Map.find v) mvertex
                 else "C" + (string << Map.find v) mvertex
    let center ((ux,uy),(vx,vy),(wx,wy)) = ((ux+vx+wx)/3N, (uy+vy+wy)/3N)
    let center' l = l |> List.fold (fun (cx,cy) (vx,vy) -> (cx+vx, cy+vy)) (0N,0N)
                      |> (fun (cx, cy) -> let n = l |> List.length
                                                    |> BigRational.FromInt
                                          (cx / n, cy / n))
    let coord = string << BigRational.ToDouble
    let vertex ((x,y) : Vertex) = coord x + ", " + coord y
    let triangle ((u,v,w) : Triangle) = let c = center (u,v,w)
                                        let l = "(" + aux1 u + "," + aux1 v + "," + aux1 w + ")"
                                        "set label at " + vertex c + " \"" + l + "\""
    let non_triangle (l : Vertex list) = let c = center' l
                                         let x = l |> List.map aux1
                                                   |> String.concat ","
                                                   |> (fun str -> "(" + str + ")")
                                         "set label at " + vertex c + " \"" + x + "\""
    let is_regular = function
        | RegularLine _ -> true
        | _ -> false
    let lines = function
        | RegularLine (m, b) -> "     " + coord m + " * x + " + coord b + " title \"\" linecolor rgb '#000000'"
        | IrregularLine x -> "     " + coord x + ", t with lines title \"\" linecolor rgb '#000000'"
    let rgb (v : Vertex) =
        if Set.contains v vertex_set
        then "'black'"
        else "'red'"
    let vertex_lbl (v : Vertex) =
          "     '+' using ($0 == 0 ? " + (coord << fst) v + " : NaN):(" + (coord << snd) v + "):('" + aux1 v + "') with labels offset char 1,-0.2 left textcolor rgb " + rgb v + " point linestyle 1 notitle"
    let header =["set terminal postscript eps enhanced color font 'Helvetica,10'"
                 "set output \"" + file + ".eps\""
                 "set format xy \"%g\""
                 "set title \"Planar Graph\""
                 "set xrange [0.0:1.0]"
                 "set yrange [0.0:1.0]"
                 "set xlabel \"\""
                 "set ylabel \"\""
                 "set style line 1 pointtype 7 linecolor rgb '#000000' pointsize 2"
                 "set multiplot"]
    let vertices = all_vertices
                    |> List.map vertex_lbl
                    |> (fun lines -> "plot " + List.head lines :: List.tail lines)
    let (regular, irregular) =
        g.lines
            |> List.partition is_regular
            |> Library.pairself (List.map lines)
    let triangles = List.map triangle g.triangles
    let non_triangles = List.map non_triangle g.non_triangles
    let separated1 = vertices @ regular 
                        |> String.concat ",\\\n"
    let separated2 = if List.isEmpty irregular
                     then ""
                     else
                     irregular
                        |> (fun lines -> "plot " + List.head lines :: List.tail lines)
                        |> String.concat ",\\\n"
    let outFile = new StreamWriter(file + ".gnuplot")
    List.iter (fun (l : string) -> outFile.WriteLine l) header
    List.iter (fun (l : string) -> outFile.WriteLine l) triangles
    List.iter (fun (l : string) -> outFile.WriteLine l) non_triangles
    outFile.WriteLine separated1
    outFile.WriteLine "set parametric"
    outFile.WriteLine separated2
    outFile.Flush()
    outFile.Close()
    ()