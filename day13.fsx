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

let timetable = getData filename |> parse
part1 timetable

