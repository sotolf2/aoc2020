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

let parse lns =
    List.map parseLine lns

let part1 nav =
    follow nav (Facing.East, (0, 0))
    |> snd
    |> (fun (row, col) -> (abs row) + (abs col))
    |> printfn "Part1: %d"

let navigation = getData filename |> parse 
