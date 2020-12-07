open System.Text.RegularExpressions

let filename = "day7.txt"

let getData filename = 
    System.IO.File.ReadAllLines filename 
    |> List.ofSeq
    
let getSubBags str =
    let pattern = @"(\d+)\s(\w+\s\w+)"
    Regex.Matches(str, pattern)
    |> Seq.cast<Match>
    |> Seq.map (fun m -> ((int m.Groups.[1].Value, m.Groups.[2].Value)))
    |> List.ofSeq


let getMainBag str =
    let pattern = @"^(\w+\s\w+)"
    Regex.Matches(str, pattern)
    |> Seq.cast<Match>
    |> Seq.tryHead
    |> Option.bind (fun m -> Some(m.Groups.[0].Value))

let parseLine str =
    ((Option.get (getMainBag str)), (getSubBags str))

let parse lns =
    List.map parseLine lns
    |> Map.ofList

let rec contains' ndl bgs bagMap =
    match bgs with
    | [] -> false
    | (hd::tl) ->
        if snd hd = ndl then
            true
        else
            let subs = Map.find (snd hd) bagMap
            contains' ndl (List.append tl subs) bagMap

let contains ndl bg bagMap =
    contains' ndl [(1, bg)] bagMap

let part1 (bagMap: Map<string, List<int*string>>) =
    Map.toList bagMap
    |> List.map fst
    |> List.except ["shiny gold"]
    |> List.map (fun bg -> contains "shiny gold" bg bagMap)
    |> List.filter id
    |> List.length
    |> printfn "Part1: %d"

let rec sepBags (cnt, desc) acc =
    match cnt with
    | 0 -> acc
    | x -> 
        sepBags (cnt - 1, desc) ((1 , desc)::acc)
    

let rec countSubBags' bgs bagMap cnt =
    match bgs with
    | [] -> cnt
    | (hd::tl) ->
        let subs = Map.find (snd hd) bagMap |> List.collect (fun bg -> sepBags bg []) 
        countSubBags' (List.append tl subs) bagMap (cnt + (fst hd))

let countSubBags bg bagMap =
    countSubBags' [(0, bg)] bagMap 0

let part2 bagMap =
    countSubBags "shiny gold" bagMap
    |> printfn "Part2: %d"

let bags = getData filename |> parse

part1 bags 
part2 bags