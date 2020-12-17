let filename = "day17.txt"

let getData filename = 
    System.IO.File.ReadAllLines filename 
    |> List.ofSeq

type Cube = Active | Inactive

let parseState = function
| '#' -> Active
| '.' -> Inactive
| ch -> failwithf "Unknown state: '%c'" ch

let parse lines =
    List.mapi (fun i ln ->
                Seq.mapi (fun j ch -> 
                        let state = parseState ch
                        ((i, j, 0), state)) ln
                |> List.ofSeq) lines
    |> List.concat
    |> Map.ofList

let getNeighbours (x, y, z) =
    seq { for x in [x-1; x; x+1] do
            for y in [y-1; y; y+1] do
                for z in [z-1; z; z+1] -> (x,y,z)}
    |> List.ofSeq
    |> List.except [(x,y,z)]

let rec addInactive lst map =
    match lst with
    | [] -> map
    | (hd::tl) ->
        match Map.tryFind hd map with
        | None -> addInactive tl (Map.add hd Inactive map)
        | Some(x) -> addInactive tl map

let expandDimension map =
    let allNeighbours = 
        Map.toList map
        |> List.collect (fst >> getNeighbours)
    addInactive allNeighbours map

let nextState map coord cur =
    let neighbours = getNeighbours coord
    let active = List.map (fun x -> Map.tryFind x map) neighbours 
                 |> List.filter (fun x -> x <> None)
                 |> List.map (fun x -> 
                                match x with 
                                | Some(x) -> x
                                | None -> failwith "None filter failed")
                 |> List.filter (fun x -> x = Active)
                 |> List.length
    match cur, active with
    | Active, 2 -> Active
    | Active, 3 -> Active
    | Active, _ -> Inactive
    | Inactive, 3 -> Active
    | Inactive, _ -> Inactive

let nextGeneration map =
    let expanded = expandDimension map
    let next = nextState expanded
    Map.map next expanded

let rec generations cnt map = 
    if cnt = 0 then map else
    generations (cnt - 1) (nextGeneration map)

let part1 =
    let dimension = getData filename |> parse
    generations 6 dimension
    |> Map.toList
    |> List.map snd
    |> List.filter (fun x -> x = Active)
    |> List.length
    |> printfn "Part1: %d"

let getNeighbours4d (x,y,z,w) =
    seq { for x in [x-1; x; x+1] do
            for y in [y-1; y; y+1] do
                for z in [z-1; z; z+1] do 
                    for w in [w-1; w; w+1] -> (x,y,z,w)}
    |> List.ofSeq
    |> List.except [(x,y,z,w)]


let parse4d lines =
    List.mapi (fun i ln ->
                Seq.mapi (fun j ch -> 
                        let state = parseState ch
                        ((i, j, 0, 0), state)) ln
                |> List.ofSeq) lines
    |> List.concat
    |> Map.ofList

let expandDimension4d map =
    let allNeighbours = 
        Map.toList map
        |> List.collect (fst >> getNeighbours4d)
    addInactive allNeighbours map


let nextState4d map coord cur =
    let neighbours = getNeighbours4d coord
    let active = List.map (fun x -> Map.tryFind x map) neighbours 
                 |> List.filter (fun x -> x <> None)
                 |> List.map (fun x -> 
                                match x with 
                                | Some(x) -> x
                                | None -> failwith "None filter failed")
                 |> List.filter (fun x -> x = Active)
                 |> List.length
    match cur, active with
    | Active, 2 -> Active
    | Active, 3 -> Active
    | Active, _ -> Inactive
    | Inactive, 3 -> Active
    | Inactive, _ -> Inactive

let nextGeneration4d map =
    let expanded = expandDimension4d map
    let next = nextState4d expanded
    Map.map next expanded

let rec generations4d cnt map = 
    if cnt = 0 then map else
    generations4d (cnt - 1) (nextGeneration4d map)

let part2 =
    let dimension = getData filename |> parse4d
    generations4d 6 dimension
    |> Map.toList
    |> List.map snd
    |> List.filter (fun x -> x = Active)
    |> List.length
    |> printfn "Part2: %d"

part1
part2
