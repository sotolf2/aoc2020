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
    printfn "ip: %d" ip
    printfn "akk: %d" akk
    printfn "op: %A" prog.[ip]
    printfn "seen: %A" seen
    if Set.contains ip seen then akk else
    match prog.[ip] with
    | NOP _ -> run prog (ip + 1) akk (Set.add ip seen)
    | ACC x -> run prog (ip + 1) (akk + x) (Set.add ip seen)
    | JMP x -> run prog (ip + x) akk (Set.add ip seen)


let execute prog =
    run prog 0 0 Set.empty
