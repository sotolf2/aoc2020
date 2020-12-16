let filename = "day16.txt"

type Field = {
    Label: string
    Range1: int list
    Range2: int list
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
        Range1 = [range1.[0]..range1.[1]]
        Range2 = [range2.[0]..range2.[1]]
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
        let setWithR1 = List.fold (fun st x -> Set.add x st) numSet hd.Range1
        let setWithR1R2 = List.fold (fun st x -> Set.add x st) setWithR1 hd.Range2
        setOfnumbers tl setWithR1R2

let part1 fields tickets =
    let validNums = setOfnumbers fields Set.empty
    List.concat tickets
    |> List.filter (fun valu -> not <| Set.contains valu validNums)
    |> List.sum
    |> printfn "Part1: %d"

let isValidTicket validNums ticket =
    not <| List.exists (fun valu -> not <| Set.contains valu validNums) ticket

let isValidCandidate field valu =
    List.contains valu field.Range1 || List.contains valu field.Range2

let trimImpossibleFields valu candidates =
    List.filter (fun cand -> isValidCandidate cand valu) candidates

let rec possibleFields tickets candidates =
    match tickets with
    | [] -> candidates
    | (hd::tl) ->
        let nuCandidates = List.map2 trimImpossibleFields hd candidates
        possibleFields tl nuCandidates

let getDeduced fieldlists =
    List.filter (fun x -> List.length x = 1) fieldlists 
    |> List.map List.head

let rec deduceFields candList =
    if List.exists (fun x -> (List.length x) > 1) candList then
        let deduced = getDeduced candList
        deduceFields (List.map (fun x ->
                                if List.length x = 1 then x
                                else List.filter (fun y ->
                                                    not <| List.contains y deduced) x) candList)
    else
        List.map List.head candList
        

let part2 fields myTicket nearbyTickets =
    let validNums = setOfnumbers fields Set.empty 
    let ticketChecker = isValidTicket validNums 
    let validTickets = List.filter ticketChecker nearbyTickets
    let candidates = List.head validTickets
                     |> List.map (fun _ -> fields)
    let prepared = possibleFields validTickets candidates
    let fieldOrder = deduceFields prepared 
                     |> List.map (fun x -> x.Label) 
    List.zip fieldOrder myTicket
    |> List.filter (fun (label,_) -> label.StartsWith("departure"))
    |> List.map (snd >> uint64)
    |> List.reduce ( * )
    |> printfn "Part2: %d"


let (fields, myTicket, nearbyTickets) = getData filename |> parse

part1 fields nearbyTickets
part2 fields myTicket nearbyTickets
