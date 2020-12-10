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

let adapters = getData filename