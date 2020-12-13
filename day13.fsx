let filename = "day13.txt"

let getData filename = 
    System.IO.File.ReadAllLines filename 
    |> List.ofSeq

let parse (lines: list<string>) =
    let arrival = uint32 (List.head lines)
    let busses = lines.[1].Split(',') 
                |> Array.filter (fun x -> x <> "x") 
                |> Array.map uint32 
                |> List.ofArray

    (arrival, busses)

let genInfinite from = 
    Seq.initInfinite (fun i -> ((uint32 i) + 1u) * from)

let part1 (arrival, busses) =
    let closestTimes =
        busses
        |> List.map (fun x -> 
                        genInfinite x 
                        |> Seq.skipWhile (fun x -> x < arrival)
                        |> Seq.head)
    
    List.zip busses closestTimes
    |> List.minBy snd
    |> (fun (bId, time) -> (time - arrival) * bId)
    |> printfn "Part1: %d"

let parse2 (lines: list<string>) =
    lines.[1].Split(',')
    |> Array.map (fun x -> if x = "x" then None else Some (uint64 x))
    |> List.ofArray
    |> List.mapi (fun i x -> (((uint64 i)), x))
    |> List.filter (fun (_,x) -> x <> None)
    |> List.map (fun (i, x) -> (i, Option.get x))

let rec searchLine (busses:list<uint64*uint64>) cur inc  =
    match busses with
    | [] -> cur
    | (hd::tl) ->
        if (cur + (fst hd)) % (snd hd) = 0UL then 
            //printfn "found place for hd: %A with cur: %d and inc: %d" hd cur inc |> ignore
            searchLine tl cur (inc * snd hd) else
        searchLine busses (cur + inc) inc 

let part2 busses =
    searchLine busses 0UL 1UL
    |> printfn "Part2: %d"

let timetable = getData filename |> parse
part1 timetable
let busses = getData filename |> parse2
part2 busses
