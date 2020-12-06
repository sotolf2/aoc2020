let filename = "day6.txt"

let getData filename = 
    System.IO.File.ReadAllLines filename 
    |> List.ofSeq

let rec parseGroup lns acc =
    match lns with
    | [] -> acc
    | (hd::tl) ->
        parseGroup tl ((Set.ofSeq hd)::acc)

let rec parseGroups lns cur acc =
    match lns with
    | [] when List.isEmpty cur -> acc
    | [] -> parseGroups [] [] ((parseGroup cur [])::acc)
    | (hd::tl) when hd = "" -> parseGroups tl [] ((parseGroup cur [])::acc)
    | (hd::tl) -> parseGroups tl (hd::cur) acc

let parse lns = 
    parseGroups lns [] []

let part1 frms =
    List.map Set.unionMany frms
    |> List.sumBy Set.count 
    |> printfn "Part1: %d"

let part2 frms =
    List.map Set.intersectMany frms
    |> List.sumBy Set.count
    |> printfn "Part2: %d"

let forms = getData filename |> parse

part1 forms
part2 forms