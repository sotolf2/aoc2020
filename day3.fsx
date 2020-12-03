let filename = "day3.txt"

let getData filename = 
    System.IO.File.ReadAllLines filename 
    |> List.ofSeq

let trans = function
| '.' -> false
| '#' -> true
| x -> failwith (sprintf "unknown character %c" x)

let parseLine line =
    Array.ofSeq line
    |> Array.map trans

let parse lines =
    List.map parseLine lines

let rec ride width col skipRow skipCol (rows : bool [] list) hit =
    match rows with
    | [] -> hit
    | (hd::tl) ->
        let nuCol = (col + skipCol) % width
        let nuHit = if hd.[nuCol] then hit + 1UL else hit
        if (List.length rows > skipRow) then
            ride width nuCol skipRow skipCol (List.skip skipRow tl) nuHit
        else
            ride width nuCol skipRow skipCol [] nuHit

let strategy right down map =
    ride (Array.length (List.head map)) -right (down - 1) right map 0UL

let part1 map = 
    printfn "Part1: %d" (strategy 3 1 map)

let part2 map =
    let strategies = [(1,1); (3,1); (5,1); (7,1); (1,2)]
    let results = List.map (fun (right,down) -> strategy right down map) strategies
    //printfn "results: %A" results
    printfn "Part2: %d" (List.reduce ( * ) results)

let map = getData filename |> parse

part1 map
part2 map