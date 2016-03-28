module Gnuplot

open Graph
open Line
open System.IO
open System

let execute_command command args =
    let p = new System.Diagnostics.Process()
    p.StartInfo.FileName <- command
    p.StartInfo.Arguments <-  args
    p.StartInfo.RedirectStandardOutput <- true
    p.StartInfo.UseShellExecute <- false
    ignore ( p.Start() )
    let output = p.StandardOutput.ReadToEnd()
    p.Close()

let graph_to_gnuplot file (g : PlanarGraph) =
    let vertex_set = set g.vertices
    let polygons = g.wingA @ g.wingB @ g.wingC
    let all_vertices = 
        polygons
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
    let polygons = List.map non_triangle (g.wingA @ g.wingB @ g.wingC)
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
    List.iter (fun (l : string) -> outFile.WriteLine l) polygons
    outFile.WriteLine separated1
    outFile.WriteLine "set parametric"
    outFile.WriteLine separated2
    outFile.Flush()
    outFile.Close()
    ()

(*
set angle degrees

# use 'square' aspect ratio else calculated angles don't match display angles
set size square

# draw a 3,4,5 right triangle as a polygon - f[ill]s[tyle] empty border 1 (red)
set object 1 poly from 1,1 to 1,4 to 5,1 to 1,1 fc rgb "grey" fs solid border 1

*)

let graph_to_gnuplot' debug file n =
  match Stack.get_best' n with
  | Some (g, succ) ->
    let vertex_set = set g.vertices
    let all_polygons = g.wingA @ g.wingB @ g.wingC
    let all_vertices = 
        all_polygons
            |> List.concat
            |> (Set.toList << set)
            |> List.partition (fun v -> Set.contains v vertex_set)
            |> (fun (x,y) -> if debug
                             then x @ y
                             else List.rev g.vertices)
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
    // set object 1 poly from 1,1 to 1,4 to 5,1 to 1,1 fc rgb "grey" fs solid border 1
    let polygon_to l = let coords = List.map (fun (ux,uy) -> coord ux + "," + coord uy) l
                       let coords = coords @ [List.head coords]
                       "set object poly from " + String.concat " to " coords + " back fc rgb \"grey\" fs solid border 1"
    let circle_to (ux,uy) = 
        "set object circle at " + coord ux + "," + coord uy + " front size 0.01 fc rgb 'black' fs solid 1"

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
    let polygons = List.map polygon_to succ
    let vertices' = List.map circle_to g.vertices
    let polygons =
        if debug
        then List.map non_triangle all_polygons
        else []
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
    List.iter (fun (l : string) -> outFile.WriteLine l) polygons
    List.iter (fun (l : string) -> outFile.WriteLine l) vertices'
    if debug
    then List.iter (fun (l : string) -> outFile.WriteLine l) polygons
    else ()
    outFile.WriteLine separated1
    outFile.WriteLine "set parametric"
    outFile.WriteLine separated2
    outFile.Flush()
    outFile.Close()
    execute_command "gnuplot" (file + ".gnuplot")
  | None -> printfn "No such graph"

let graph_to_animation file n =
//    execute_command "rm" (file + "*.*")
    List.iter (fun n -> graph_to_gnuplot' false (file + (sprintf "%02d" n)) (uint32 n)) [3..n]
    execute_command "convert" ("-size 500x500 -density 500 -quality 100 -set delay 100 -colorspace GRAY -colors 256 -dispose 2 -loop 0 -scale 300% *.eps " + file + ".gif")
      
