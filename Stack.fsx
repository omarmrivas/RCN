module Stack

#r "SQLProvider/FSharp.Data.SQLProvider.dll"
//#r "packages/MySql.Data.6.9.8/lib/net45/MySql.Data.dll"

open System
//open System.Data
open FSharp.Data.Sql
open FSharp.Data.Sql.Providers
open MySql
open MySql.Data
open MySql.Data.MySqlClient
open Graph
open Library

[<Literal>]
let resPath     = @"packages/MySql.Data.6.9.8/lib/net45"

[<Literal>]
let connString  = "Server=localhost;Database=rcn;User=rcn;Password=rcn"

type Sql = 
    SqlDataProvider< 
        ConnectionString=connString,
        DatabaseVendor=Common.DatabaseProviderTypes.MYSQL,
        ResolutionPath = resPath,
        IndividualsAmount = 1000,
        UseOptionTypes = true,
        Owner="rcn">

let ctxt = Sql.GetDataContext()

let connection = new MySql.Data.MySqlClient.MySqlConnection(connString)

let delete id =
    query {for e in ctxt.Rcn.Stack do
           where (e.Id = id)
           select e
           exactlyOne}
           |> (fun e -> e.Delete())
    ctxt.SubmitUpdates()

let empty () =
    Seq.isEmpty ctxt.Rcn.Stack

let array_to_string (data : byte []) =
    let hex = new System.Text.StringBuilder(Array.length data * 2)
    ignore (hex.Append [|'0'; 'x'|])
    for b in data do
        ignore (hex.AppendFormat("{0:x2}", b))
    hex.ToString()

let push ((depth : uint32), (g : PlanarGraph), (solution : Vertex list)) =
    connection.Open()
    let commandTxt = sprintf "INSERT INTO `Stack`(`depth`, `vertices`, `graph`, `solution`, `triangles`, `polygons`, `crossings`) VALUES (%d,%d,?graph,?solution,%d,%d,%d)"
                             depth (List.length g.vertices) (List.length g.triangles) (List.length g.non_triangles) g.crossing_number
    let command = new MySqlCommand(commandTxt, connection)
    let s_graph = Library.serialize g
    let s_solution = Library.serialize solution
    let p_graph = new MySqlParameter("?graph", MySqlDbType.LongBlob, Array.length s_graph)
    let p_solution = new MySqlParameter("?solution", MySqlDbType.MediumBlob, Array.length s_solution)
    p_graph.Value <- s_graph
    p_solution.Value <- s_solution
    ignore (command.Parameters.Add p_graph)
    ignore (command.Parameters.Add p_solution)
    ignore (command.ExecuteNonQuery())
    connection.Close()

let get_best n =
    let bests = query {for e in ctxt.Rcn.Graphs do
                       where (e.Vertices = n)
                       select e}
    if Seq.isEmpty bests
    then None
    else (Seq.head bests).Solution
            |> Library.deserialize<Vertex list>
            |> Some

let get_best' n =
    let bests = query {for e in ctxt.Rcn.Graphs do
                       where (e.Vertices = n)
                       select e}
    if Seq.isEmpty bests
    then None
    else let best = Seq.head bests
         let g = Library.deserialize<PlanarGraph> best.Graph
         let succ = Library.deserialize<Graph.Vertex list list> best.Succesors
         Some (g, succ)

let update_best (succesors : Graph.Vertex list list) ((g : PlanarGraph), (solution : Vertex list)) =
    let n = uint32 (List.length g.vertices)
    let bests = query {for e in ctxt.Rcn.Graphs do
                       where (e.Vertices = n)
                       select e}
                       |> Seq.toList
    if List.isEmpty bests || uint32 g.crossing_number <= (List.head bests).Crossings
    then printfn "updating best"
         connection.Open()
         let commandTxt = sprintf "INSERT INTO `Graphs`(`vertices`, `graph`, `solution`, `succesors`, `triangles`, `polygons`, `crossings`) VALUES (%d,?graph,?solution,?succesors,%d,%d,%d)"
                                  (List.length g.vertices) (List.length g.triangles) (List.length g.non_triangles) g.crossing_number
         let command = new MySqlCommand(commandTxt, connection)
         let s_graph = Library.serialize g
         let s_solution = Library.serialize solution
         let s_succesors = Library.serialize succesors
         let p_graph = new MySqlParameter("?graph", MySqlDbType.LongBlob, Array.length s_graph)
         let p_solution = new MySqlParameter("?solution", MySqlDbType.MediumBlob, Array.length s_solution)
         let p_succesors = new MySqlParameter("?succesors", MySqlDbType.MediumBlob, Array.length s_succesors)
         p_graph.Value <- s_graph
         p_solution.Value <- s_solution
         p_succesors.Value <- s_succesors
         ignore (command.Parameters.Add p_graph)
         ignore (command.Parameters.Add p_solution)
         ignore (command.Parameters.Add p_succesors)
         ignore (command.ExecuteNonQuery())
         connection.Close()
         // delete previous best
         if List.isEmpty bests
         then ()
         else (List.head bests).Delete()
              ctxt.SubmitUpdates()
    else ()

let peak () =
    if empty ()
    then None
    else let id = ctxt.Functions.Maximum.Invoke().ReturnValue
         query {
            for e in ctxt.Rcn.Stack do
            where (e.Id = id)
            select e
            exactlyOne}
           |> (fun e -> let g = Library.deserialize<PlanarGraph> e.Graph
                        let s = Library.deserialize<Vertex list> e.Solution
                        (e.Id, e.Depth, g, s))
           |> Some

let pop () =
    match peak () with
        | Some (id, depth, g, solution) -> 
            delete id
            Some (depth, g, solution)
        | None -> None
