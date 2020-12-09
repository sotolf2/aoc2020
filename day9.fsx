let filename = "day9.txt"

let getData filename = 
    System.IO.File.ReadAllLines filename 
    |> List.ofSeq
    |> List.map int64

let data: list<int64>= getData filename

let rec getSums (nums: list<int64>) acc =
    match nums with
    | [] -> acc
    | [_] -> acc
    | (hd::tl) ->
        let nuSet = 
            List.map (fun x -> x + hd) tl
            |> List.fold (fun st x -> Set.add x st) acc 
        getSums tl nuSet   

let isPairsum pream cur =
    let sums = getSums pream Set.empty
    Set.contains cur sums

let rec searchStream' pream cur nxt =
    match nxt with
    | [] -> if not <| isPairsum pream cur then Some cur else None
    | (hd::tl) ->
        if not <| isPairsum pream cur then Some cur else
        searchStream' ((List.tail pream) @ [cur]) hd tl

let searchStream lst lenpream = 
    let pream = List.take lenpream lst
    let rest = List.skip lenpream lst
    searchStream' pream (List.head rest) (List.tail rest)

let part1 strm =
    searchStream strm 25
    |> Option.get
    |> printfn "Part1: %d"

let goal = 507622668L

let rec searchContiguous' (goal : int64) masterlst lst curset =
    if List.sum curset = goal then curset else
    if List.sum curset > goal then searchContiguous' goal (List.tail masterlst) (List.tail masterlst) [] else
    searchContiguous' goal masterlst (List.tail lst) ((List.head lst)::curset)

let searchContiguous goal lst =
    searchContiguous' goal lst lst []

let minmax lst =
    (List.min lst, List.max lst)

let part2 goal strm =
    searchContiguous goal strm 
    |> minmax
    |> (fun (mn, mx) -> mn + mx)
    |> printfn "Part2: %d"

part1 data
part2 goal data 