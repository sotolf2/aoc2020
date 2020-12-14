open System.Text.RegularExpressions

let filename = "day14.txt"

type Memory = {
    Address: uint64
    Value: uint64
}

type Instruction = 
    | Mask of string
    | Mem of Memory

let getData filename = 
    System.IO.File.ReadAllLines filename 
    |> List.ofSeq

let parseMask (ln: string) = 
    ln.Split ' '
    |> Array.last
    |> Mask

let parseMemory (ln: string)= 
    let pattern = @"mem\[(\d+)] = (\d+)"
    Regex.Matches(ln, pattern)
    |> Seq.cast<Match>
    |> Seq.head
    |> (fun m -> ((uint64 m.Groups.[1].Value, uint64 m.Groups.[2].Value)))
    |> (fun (address, value) -> {Address = address; Value = value})
    |> Mem

let parseLine (ln: string) =
    match ln.Substring(0,4) with
    | "mask" -> parseMask ln
    | "mem[" -> parseMemory ln
    | start -> failwithf "Unknown match: %s in line: %s" start ln

let rec ultb num (acc: seq<char>) = 
    match num with
    | 0UL -> System.String.Concat(acc)
    | n when n < 2UL -> ultb 0UL (Seq.append (sprintf "%d" n) acc)
    | n -> 
        let rem = n % 2UL
        ultb (n / 2UL) (Seq.append (sprintf "%d" rem) acc)

let ULToBinary num =
    ultb num ""
    |> (fun str -> str.PadLeft(36,'0'))

let rec stul (revstr: seq<char>) divid acc =
    match revstr with
    | sq when Seq.isEmpty sq -> acc
    | sq when Seq.head sq = '1' -> stul (Seq.tail sq) (divid * 2UL) (acc + divid)
    | sq when Seq.head sq = '0' -> stul (Seq.tail sq) (divid * 2UL) (acc)
    | sq -> failwithf "%c not allowed in binary string" (Seq.head sq)


let bstrToUL (str: string) =
    let trimmed = str.TrimStart('0')
    stul (Seq.rev trimmed) 1UL 0UL


let masknum num mask =
    let binrep = ULToBinary num
    Seq.zip mask binrep
    |> Seq.map (fun (m, v) -> if m = 'X' then v else m)
    |> (System.String.Concat >> bstrToUL)

let parse lns =
    List.map parseLine lns

let rec runInit prog mask memory =
    match prog with
    | [] -> memory
    | (hd::tl) ->
        match hd with
        | Mask msk -> runInit tl msk memory 
        | Mem mem -> runInit tl mask (Map.add mem.Address (masknum mem.Value mask) memory) 

let part1 prog =
    runInit prog "" Map.empty
    |> Map.toList
    |> List.sumBy snd
    |> printfn "Part1: %d"

let rec dupFirstX lst head =
    match lst with
    | [] -> [List.append (List.rev head) lst]
    | (hd::tl) when (fst hd = 'X') ->
        let start = List.rev head
        [
            List.concat [start; [('0', '1')]; tl];
            List.concat [start; [('0', '0')]; tl];
        ]
    | (hd::tl) ->
        dupFirstX tl (hd::head)

let rec rewrite (lst: (char*char) list list) acc =
    match lst with
    | [] -> acc
    | (hd::tl) ->
        if List.exists (fun (mch, _) -> mch = 'X') hd then
            rewrite (List.append (dupFirstX hd []) tl) acc
        else
            rewrite tl (hd::acc)

let getAddresses (address: seq<char>) mask = 
    rewrite [(Seq.zip mask address |> List.ofSeq)] [] 

let decodeAddresses address mask =
    let binrep = ULToBinary address
    getAddresses binrep mask
    |> List.map (fun addr ->
                    List.map (fun (msk, bt) -> if msk = '1' then '1' else bt) addr
                    |> (fun lst -> System.String.Concat(lst))
                    |> bstrToUL)

let rec runMemAddDec prog mask memory =
    match prog with
    | [] -> memory
    | (hd::tl) ->
        match hd with
        | Mask msk -> runMemAddDec tl msk memory 
        | Mem mem ->
            let numap = decodeAddresses mem.Address mask
                        |> List.fold (fun st addr -> Map.add addr mem.Value st) memory
            runMemAddDec tl mask numap
            
let part2 prog =
    runMemAddDec prog "" Map.empty
    |> Map.toList
    |> List.sumBy snd
    |> printfn "Part2: %d"

let prog = getData filename |> parse

part1 prog
part2 prog