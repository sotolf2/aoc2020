let filename = "day10t.txt"

let getData filename = 
    System.IO.File.ReadAllLines filename 
    |> List.ofSeq
    |> List.map int

let addSocketDevice lst =
    let last = (List.last lst) + 3
    (0::(lst @ [last]))

let diffCounts adapters =
    List.sort adapters
    |> addSocketDevice
    |> List.pairwise
    |> List.map (fun (x, y) -> y - x)
    |> List.countBy id

let part1 adapters =
    diffCounts adapters
    |> List.filter (fun (x, _) -> (x = 1) || (x = 3))
    |> List.map snd
    |> List.reduce ( * )
    |> printfn "Part1: %d"

let differences lst =
    List.pairwise lst
    |> List.map (fun (x,y) -> y - x)

let rec splitConsec lst cur acc =
    match lst with
    | [] -> if List.isEmpty cur then acc else
            List.rev (cur::acc) 
    | (hd::tl) ->
        if hd = 1 then splitConsec tl (hd::cur) acc else
        if List.isEmpty cur then splitConsec tl [] acc else
        splitConsec tl [] (cur::acc)

// my test data has no runs over 3 so I can cheat
let combs = function
| 1UL -> 1UL
| 2UL -> 2UL
| 3UL -> 4UL
| 4UL -> 7UL
| x -> failwithf "found run of %d" x

let part2 data =
    differences data
    |> (fun x -> splitConsec x [] [])
    |> List.map (List.length >> (fun x -> uint64(x)) >> combs)
    |> List.reduce ( * )
    |> printfn "Part2: %d"



let data = getData "day10.txt" |> List.sort |> addSocketDevice

part1 data
part2 data

