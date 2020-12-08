let filename = "day8.txt"

type Op = NOP of int | ACC of int | JMP of int

let getData filename = 
    System.IO.File.ReadAllLines filename 
    |> List.ofSeq

let parseLine (ln:string) =
    let parts = ln.Split()
    match parts.[0] with
    | "nop" -> NOP (int parts.[1])
    | "acc" -> ACC (int parts.[1])
    | "jmp" -> JMP (int parts.[1])
    | str -> failwith (sprintf "unknown op %s" str)

let parse lns =
    List.map parseLine lns
    |> Array.ofList

let rec run (prog: Op []) ip akk seen =
    if Set.contains ip seen then (akk, false) else
    if ip = Array.length prog then (akk, true) else
    if ip > Array.length prog then (0, false) else
    match prog.[ip] with
    | NOP _ -> run prog (ip + 1) akk (Set.add ip seen)
    | ACC x -> run prog (ip + 1) (akk + x) (Set.add ip seen)
    | JMP x -> run prog (ip + x) akk (Set.add ip seen)

let execute prog =
    run prog 0 0 Set.empty

let part1 code =
    execute code
    |> fst
    |> printfn "Part1: %d"

let code = getData filename |> parse

let rec fixcode (code: Op []) ip =
    if ip > Array.length code then 0 else
    let nuCode = match code.[ip] with
                    | NOP x -> Array.mapi (fun i elm -> if i = ip then (JMP x) else elm) code 
                    | JMP x -> Array.mapi (fun i elm -> if i = ip then (NOP x) else elm) code 
                    | _ -> code
    
    let res = execute nuCode

    if snd res then fst res else
    fixcode code (ip + 1)

let part2 code =
    fixcode code 0
    |> printfn "Part2: %d"     

part1 code
part2 code