let filename = "day12.txt"

let getData filename = 
    System.IO.File.ReadAllLines filename 
    |> List.ofSeq

type Action =
    North of int
    | South of int
    | East of int
    | West of int
    | Left of int
    | Right of int
    | Forward of int

type Facing = North = 0 | East = 1 | South = 2 | West = 3
type Dir = L | R

let toAction letter number =
    match letter with
    | "N" -> North number
    | "S" -> South number
    | "E" -> East number
    | "W" -> West number
    | "L" -> Left number
    | "R" -> Right number
    | "F" -> Forward number
    | str -> failwithf "Unknown action %s" str

let parseLine (ln: string) =
    let letter = ln.Substring(0,1)
    let number = ln.Substring(1) |> int
    toAction letter number

let parse lns =
    List.map parseLine lns

let modulo n m = ((n % m) + m) % m

let turn dir amount fac =
    let times = amount / 90
    let nuint = match dir with
                | L -> modulo ((int fac) - times) 4
                | R -> modulo ((int fac) + times) 4
    enum<Facing>(nuint)

let forward (fac: Facing) n (row, col) =
    match fac with
    | Facing.North -> (fac, (row + n, col))
    | Facing.East -> (fac, (row, col + n))
    | Facing.South -> (fac, (row - n, col))
    | Facing.West -> (fac, (row, col - n))
    | err -> failwithf "Nonexistant facing %A" err 


let move act (fac, (row, col)) =
    match act with
    | North(n) -> (fac, (row + n, col))
    | South(n) -> (fac, (row - n, col))
    | East(n) -> (fac, (row, col + n))
    | West(n) -> (fac, (row, col - n))
    | Left(n) -> ((turn L n fac), (row,col))
    | Right(n) -> ((turn R n fac), (row,col))
    | Forward(n) -> forward fac n (row, col)

let rec follow acts st =
    match acts with
    | [] -> st
    | (hd::tl) -> follow tl (move hd st)

let part1 nav =
    follow nav (Facing.East, (0, 0))
    |> snd
    |> (fun (row, col) -> (abs row) + (abs col))
    |> printfn "Part1: %d"

let addp (row, col) (row2, col2) =
    ((row + row2), (col + col2))

let rec rotL times wrow wcol row col =
    if times = 0 then ((wrow, wcol), (row, col)) else
    rotL (times - 1) wcol -wrow row col

let rotateLeft deg ((wrow, wcol), (row, col)) =
    let times = modulo (deg / 90) 4
    rotL times wrow wcol row col 

let rec rotR times wrow wcol row col =
    if times = 0 then ((wrow, wcol), (row, col)) else
    rotR (times - 1) -wcol wrow row col

let rotateRight deg ((wrow, wcol), (row, col)) =
    let times = modulo (deg / 90) 4
    rotR times wrow wcol row col

let rec goto n (wpt, boat) =
    if n = 0 then (wpt, boat) else
    goto (n - 1) (wpt, (addp boat wpt))

let waypoint act ((wrow, wcol), (row, col)) =
    match act with
    | North(n) -> ((wrow + n), wcol), (row, col)
    | South(n) -> ((wrow - n), wcol), (row, col)
    | East(n) -> ((wrow, (wcol + n)), (row, col))
    | West(n) -> ((wrow, (wcol - n)), (row, col))
    | Left(n) -> rotateLeft n ((wrow, wcol), (row, col))
    | Right(n) -> rotateRight n ((wrow, wcol), (row, col))
    | Forward(n) -> goto n ((wrow, wcol), (row, col))

let rec follow2 acts st =
    match acts with
    | [] -> st
    | (hd::tl) -> follow2 tl (waypoint hd st)

let part2 nav =
    follow2 nav ((1, 10), (0, 0))
    |> snd
    |> (fun (row, col) -> (abs row) + (abs col))
    |> printfn "Part2: %d"

let navigation = getData filename |> parse 

part1 navigation
part2 navigation
