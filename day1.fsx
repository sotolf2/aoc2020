open System

let filename = "day1.txt"

let getData filename = 
    System.IO.File.ReadAllLines filename 
    |> List.ofSeq

let parse (lines : list<string>) =
    List.map int lines

let findSum ndl sum hstk =
    List.tryFind (fun x -> x + ndl = sum) hstk
    |> Option.bind (fun x -> Some (ndl, x))

let rec searchSum lst =
    match lst with
    | [] -> failwith "No answer found"
    | (hd::tl) ->
        match (findSum hd 2020 tl) with
        | None -> searchSum tl
        | Some (x, y) -> (x, y)

let part1 nums = 
    let (x, y) = searchSum nums
    printfn "Part1: %d" (x*y)

let combinations sz lst =
    let rec choose lo n arr =
        match n with
        | 0 -> [[]]
        | i -> [for j=lo to (Array.length arr) - 1 do
                for ks in choose (j+1) (i-1) arr do
                yield arr.[j] :: ks]
    choose 0 sz (Array.ofList lst)

let searchSumSz sz lst =
    let rec loop lst =
        match lst with
        | [] -> failwith "No combination found"
        | (hd::tl) ->
            if (List.sum hd) = 2020 then (hd) else
            loop tl
    loop (combinations sz lst)

let sumOf3 lst =
    let res = [for x in lst do
               for y in lst do
               for z in lst do
               if x + y + z = 2020 then yield (x * y * z)]
    res.Head

let part2 nums =
    printfn "Part2: %d" (sumOf3 nums)

let nums = getData filename |> parse

part1 nums
part2 nums