module Line

open Microsoft.FSharp.Math

type vector2D = BigRational * BigRational

[<CustomEquality; CustomComparison>]
type line =
     | RegularLine of BigRational * BigRational
     | IrregularLine of BigRational

     // Smallest lines have slope 0
     // bigger ones move anticlockwise
     static member Compare(p,q) =
            match (p, q) with
                | (RegularLine (m1, _), RegularLine (m2, _)) ->
                    (*if m1 > m2 then 1
                    else if m1 < m2 then -1
                    else 0*)
                    if m1 >= BigRational.Zero
                    then if m2 >= BigRational.Zero
                         then if m1 > m2 then 1
                              elif m1 < m2 then -1
                              else 0
                         else -1
                    else if m2 >= BigRational.Zero
                         then 1
                         else if m1 > m2 then 1
                              elif m1 < m2 then -1
                              else 0
                | (RegularLine (m, _), IrregularLine _) ->
                    if m >= BigRational.Zero
                    then -1
                    else 1
                | (IrregularLine _, RegularLine (m, _)) ->
                    if m >= BigRational.Zero
                    then 1
                    else -1
                | (IrregularLine _, IrregularLine _) -> 0

     static member Equals(p, q) = 0 = line.Compare(p, q)

     interface System.IComparable with
            member this.CompareTo(obj:obj) =
                match obj with
                | :? line as that -> line.Compare(this,that)
                | _ -> invalidArg "obj" "the object does not have the correct type"

     override this.Equals(that:obj) =
            match that with
            | :? line as that -> line.Equals(this,that)
            | _ -> false

     override x.GetHashCode() =
            match x with
               | RegularLine (m, _) -> m.GetHashCode()
               | IrregularLine _ -> 21

//exception PARALLEL_LINES of line * line

let construct_line (x1, y1) (x2, y2) =
    let dx = x2 - x1
    if dx = BigRational.Zero
       then IrregularLine x1
       else let dy = y2 - y1
            let m = dy / dx
            let b = y1 + (m * (-x1))
            in RegularLine (m, b)
                
let projection line ((x, y) : vector2D) =
    // Projection of v onto s
    // s is not the Zero vector
    let projection_aux ((vx, vy) : vector2D) (sx, sy) = 
        let c = (vx * sx + vy * sy) / (sx * sx + sy * sy)
        (c * sx, c * sy)
    match line with
        | RegularLine (m, b) -> let (px, py) = projection_aux (x, y - b) (BigRational.One, m)
                                (px, py + b)
        | IrregularLine xl -> (xl, y)

let middle_line line1 line2 = 
    match (line1, line2) with
    | (RegularLine (m1, _), RegularLine (m2, _)) -> if m1 >= BigRational.Zero
                                                    then if m2 >= BigRational.Zero
                                                         then RegularLine ((m1 + m2) / 2N, BigRational.Zero)
                                                         else RegularLine (m1 + 1N, BigRational.Zero)
                                                    else if m2 >= BigRational.Zero
                                                         then RegularLine (m1 / 2N, BigRational.Zero)
                                                         else RegularLine ((m1 + m2) / 2N, BigRational.Zero)
    | (RegularLine (m, _), IrregularLine _) ->
        if m >= 0N
        then RegularLine (m + 1N, BigRational.Zero)
        else RegularLine (m - 1N, BigRational.Zero)
    | (IrregularLine _, RegularLine (m, _)) ->
        if m >= 0N
        then RegularLine (m + 1N, BigRational.Zero)
        else RegularLine (m - 1N, BigRational.Zero)
    | (IrregularLine _, IrregularLine _) -> IrregularLine BigRational.Zero

let perpendicular_line line =
    match line with
       | RegularLine (m, _) -> if m <> 0N 
                               then RegularLine (-1N / m, 0N)
                               else IrregularLine 0N
       | IrregularLine _ -> RegularLine (0N, 0N)

let circular_lines n =
    if n <= 1 then failwith "Argument must be greater than 1"
    let dx = 2N / (BigRational.FromInt(n) - 1N);
    let dy = dx
    let rec points n x y = 
        if n <= 0 then []
        else if x > 0N && (x - dx) < 0N then (x, y) :: points (n - 1) (x - dx) y
             elif x > 0N then (x, y) :: points (n - 1) (x - dx) (y + dy)
             else (x, y) :: points (n - 1) (x - dx) (y - dy)
    0N |> points n 1N
       |> List.map (construct_line (0N, 0N))
       |> set
       |> Set.toList

type side = LEFT | RIGHT | ANY

let find_side (x, y) line =
    match line with
        | RegularLine (m, b) -> let sum = (y - b) - m * x
                                if sum.IsNegative then RIGHT
                                else if sum.IsPositive then LEFT
                                else ANY
        | IrregularLine xx -> let sum = xx - x 
                              if sum.IsNegative then RIGHT
                              else if sum.IsPositive then LEFT
                              else ANY
                                
let crossing_point = function
    | (RegularLine (m1, b1), RegularLine (m2, b2)) ->
        if m1 = m2 then None//raise (PARALLEL_LINES (RegularLine (m1, b1), RegularLine (m2, b2)))
        else let x = (b2 - b1) / (m1 - m2)
             Some (x, m1 * x + b1)
    | (RegularLine (m, b), IrregularLine x) -> Some (x, m * x + b)
    | (IrregularLine x, RegularLine (m, b)) -> Some (x, m * x + b)
    | (IrregularLine x1, IrregularLine x2) -> 
        None //raise (PARALLEL_LINES (IrregularLine x1, IrregularLine x2))

let v2d_add (v1x: BigRational, v1y: BigRational) (v2x, v2y) = (v1x + v2x, v1y + v2y)
let v2d_sub (v1x, v1y) (v2x, v2y) = (v1x - v2x, v1y - v2y)
let v2d_dot (v1x, v1y) (v2x, v2y) = v1x * v2x + v1y * v2y
let v2d_mul (alpha : BigRational) (x, y) = (alpha * x, alpha * y)

let line_crosses_triangle (u,v,w) l =
    let inside (u, v) = function
        | Some p -> let uv = v2d_sub u v
                    let d = v2d_dot uv uv
                    if d = 0N
                    then printfn "Invalid triangle! %A" (u,v,w)
                         false
                    else let alpha = (v2d_dot (v2d_sub p v) uv) / d
                         alpha > 0N && alpha < 1N
        | None -> false
    inside (u, v) (crossing_point (l, construct_line u v)) ||
    inside (u, w) (crossing_point (l, construct_line u w)) ||
    inside (v, w) (crossing_point (l, construct_line v w))
