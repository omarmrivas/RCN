module OswinPage

open FSharp.Data

type crossings = HtmlProvider<"http://www.ist.tugraz.at/staff/aichholzer/research/rp/triangulations/crossing/">

let min_crossings_so_far (data : crossings) n =
    if n < 100
    then data.Tables.``On the Rectilinear Crossing Number``.Rows.[n - 1].Column2
            |> int
    else failwith "ERROR"
