open System.Text.RegularExpressions

let filename = "day4.txt"

let getData filename = 
    System.IO.File.ReadAllLines filename 
    |> List.ofSeq

let parseElem (str : string) =
    let parts = str.Split ':'
    (parts.[0], parts.[1])

let addLine (ln : string) doc =
    let parts = ln.Split()
    Array.map parseElem parts
    |> Array.fold (fun state (key,valu) -> Map.add key valu state ) doc
    

let rec parseDocument strs doc =
    match strs with
    | [] -> doc
    | (hd::tl) ->
        parseDocument tl (addLine hd doc)

let rec parseBatch btch cur acc =
    match btch with
    | [] -> (parseDocument cur Map.empty)::acc
    | (hd::tl) ->
        if hd = "" then
            parseBatch tl [] ((parseDocument cur Map.empty)::acc)
        else
            parseBatch tl (hd::cur) acc

let parseFile lines =
    parseBatch lines [] []

let rec validDocWith fields doc =
    match fields with
    | [] -> true
    | (hd::tl) ->
        if Map.containsKey hd doc then
            validDocWith tl doc 
        else
            false

let countValidFields docs =
    let reqFields = ["byr"; "iyr"; "eyr"; "hgt"; "hcl"; "ecl"; "pid"]
    List.filter (validDocWith reqFields) docs
    |> List.length


let part1 docs =
    printfn "Part1: %d" (countValidFields docs)

let checkYr lo hi key doc = 
    let valu = Map.tryFind key doc
    match valu with
    | None -> false
    | Some(yr) -> (String.length yr = 4) && lo <= (int yr) && hi >= (int yr) 

let checkByr doc =
    checkYr 1920 2002 "byr" doc

let checkIyr doc =
    checkYr 2010 2020 "iyr" doc

let checkEyr doc =
    checkYr 2020 2030 "eyr" doc

let parseHgt hgt =
    let pattern = @"^(?<digits>\d+)(?<type>(in|cm))$"
    Regex.Matches(hgt, pattern)
    |> Seq.cast<Match>
    |> Seq.tryHead
    |> Option.bind (fun m -> Some(int (m.Groups.["digits"].Value), m.Groups.["type"].Value))
    

let checkHgt doc =
    let valu = Map.tryFind "hgt" doc
    match valu with
    | None -> false
    | Some(hgt) -> 
        match parseHgt hgt with
        | None -> false
        | Some(digs, msm) ->
            match msm with
            | "cm" -> (digs >= 150) && (digs <= 193)
            | "in" -> (digs >= 59) && (digs <= 76)
            | _ -> false

let checkHcl doc =
    let pattern = @"^\#[a-f0-9]{6}$"
    let valu = Map.tryFind "hcl" doc
    match valu with
    | None -> false
    | Some(hcl) ->
        Regex.IsMatch(hcl, pattern)

let checkEcl doc =
    let valu = Map.tryFind "ecl" doc
    match valu with
    | None -> false
    | Some("amb") -> true
    | Some("blu") -> true
    | Some("brn") -> true
    | Some("gry") -> true
    | Some("grn") -> true
    | Some("hzl") -> true
    | Some("oth") -> true
    | Some(_) -> false

let checkPid doc =
    let pattern = @"^[0-9]{9}$"
    let valu = Map.tryFind "pid" doc
    match valu with
    | None -> false
    | Some(pid) ->
        Regex.IsMatch(pid, pattern)

let checkValid doc =
    [checkByr; checkEcl; checkEyr; checkHcl; checkHgt; checkIyr; checkPid]
    |> List.map (fun f -> f doc)
    |> (not << List.exists (not))

let part2 docs =
    let valids = List.length (List.filter checkValid docs)
    printfn "Part2: %d" valids


let docs = getData filename |> parseFile

part1 docs
part2 docs
