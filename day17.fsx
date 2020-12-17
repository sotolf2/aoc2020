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
    [
    // z = -1
    (x + 1, y - 1, z - 1); (x + 1, y, z - 1); (x + 1, y + 1, z - 1);
    (x    , y - 1, z - 1); (x    , y, z - 1); (x    , y + 1, z - 1);
    (x - 1, y - 1, z - 1); (x - 1, y, z - 1); (x - 1, y + 1, z - 1);
    // z = 0
    (x + 1, y - 1, z); (x + 1, y, z); (x + 1, y + 1, z);
    (x    , y - 1, z);                (x    , y + 1, z);
    (x - 1, y - 1, z); (x - 1, y, z); (x - 1, y + 1, z);
    // z = +1
    (x + 1, y - 1, z + 1); (x + 1, y, z + 1); (x + 1, y + 1, z + 1);
    (x    , y - 1, z + 1); (x    , y, z + 1); (x    , y + 1, z + 1);
    (x - 1, y - 1, z + 1); (x - 1, y, z + 1); (x - 1, y + 1, z + 1);
    ]

let rec addInactive (lst: List<(int*int*int)>) (map: Map<(int*int*int), Cube>)=
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

let part1 dimension =
    generations 6 dimension
    |> Map.toList
    |> List.map snd
    |> List.filter (fun x -> x = Active)
    |> List.length
    |> printfn "Part1: %d"
