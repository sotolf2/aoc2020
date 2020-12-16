let filename = "day16.txt"

type Field = {
    Label: string
    Range1: (int*int)
    Range2: (int*int)
}

let getData filename = 
    System.IO.File.ReadAllLines filename 
    |> List.ofSeq

let rec revSplitOnEmpty lst cur acc =
    match lst with
    | [] -> (cur::acc)
    | (hd::tl) ->
        if hd = "" then revSplitOnEmpty tl [] (cur::acc) else
        revSplitOnEmpty tl (hd::cur) acc 

let parseField (ln: string) =
    let labelRanges = ln.Split(':')
    let ranges = labelRanges.[1].Split(' ')
    let range1 = Array.map int (ranges.[1].Split('-'))
    let range2 = Array.map int (ranges.[3].Split('-'))
    {
        Label = labelRanges.[0]
        Range1 = (range1.[0], range1.[1])
        Range2 = (range2.[0], range2.[1])
    }

let parseMyTicket (lns: string list) =
    Array.map int (lns.[1].Split(',')) 
    |> List.ofArray

let parseOtherTickets (lns: string list) =
    List.skip 1 lns
    |> List.map (fun x -> Array.map int (x.Split(',')) |> List.ofArray)
    
let parse lst =
    let parts = revSplitOnEmpty (List.rev lst) [] []
    let rules = List.map parseField parts.[0]
    let myTicket = parseMyTicket parts.[1]
    let otherTickets = parseOtherTickets parts.[2]
    (rules, myTicket, otherTickets)

let rec setOfnumbers fields numSet =
    match fields with
    | [] -> numSet
    | (hd::tl) ->
        let (r1f, r1t) = hd.Range1
        let (r2f, r2t) = hd.Range2
        let setWithR1 = List.fold (fun st x -> Set.add x st) numSet [r1f..r1t]
        let setWithR1R2 = List.fold (fun st x -> Set.add x st) setWithR1 [r2f..r2t]
        setOfnumbers tl setWithR1R2

let part1 fields tickets =
    let validNums = setOfnumbers fields Set.empty
    List.concat tickets
    |> List.filter (fun valu -> not <| Set.contains valu validNums)
    |> List.sum
    |> printfn "Part1: %d"

let (fields, myTicket, nearbyTickets) = getData filename |> parse

part1 fields nearbyTickets
