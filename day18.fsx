
let filename = "day18.txt"

let getData filename = 
    System.IO.File.ReadAllLines filename 
    |> List.ofSeq

type Token =
    | Number of uint64
    | Add
    | Multiply
    | OpenGroup
    | CloseGroup

let chToToken = function
| '+' -> Add
| '*' -> Multiply
| '(' -> OpenGroup
| ')' -> CloseGroup
| ch when System.Char.IsDigit ch -> Number (uint64 (System.Char.ToString ch))
| ch -> failwithf "Unknown character: %c" ch

let rec eater chars tokens =
    match chars with
    | [] -> List.rev tokens
    | (hd::tl) ->
        if hd = ' ' then eater tl tokens else
        eater tl ((chToToken hd)::tokens)

let tokenizeLine ln = 
    List.ofSeq ln
    |> (fun ln -> eater ln [])

let tokenize lines =
    List.map tokenizeLine lines

let rec splitAtOpen tokens before =
    match tokens with
    | [] -> (before, [])
    | (hd::tl) ->
        match hd with
        | OpenGroup -> (before, (OpenGroup::tl))
        | x -> splitAtOpen tl (x::before)

let rec toRPN' tokens output stack =
    //printfn "------\ntokens: %A\noutput: %A\nstack: %A\n------" tokens output stack
    match tokens with
    | [] -> List.append (List.rev stack) output |> List.rev
    | (hd::tl) ->
        let (before, after) = splitAtOpen stack []
        match hd with
        | Number(x) -> toRPN' tl ((Number(x))::output) stack
        | Add -> 
            toRPN' tl (List.append before output) (Add::after)
        | Multiply -> 
            toRPN' tl (List.append before output) (Multiply::after)
        | OpenGroup -> toRPN' tl output (OpenGroup::stack)
        | CloseGroup ->
            toRPN' tl (List.append before output) (List.tail after)

// 1 + ( 2 * ( 3 + 4 ) + 5) -> 1 2 3 4 + * 5 +
let toRPN tokens =
    toRPN' tokens [] []

let rec evaluateRPN' tokens stack =
    match tokens with
    | [] -> stack
    | (hd::tl) ->
        match hd with
        | Number(x) -> evaluateRPN' tl (x::stack)
        | Add -> 
            match stack with
            | (x::y::rest) ->
                evaluateRPN' tl ((x + y)::rest)
            | _ -> failwith "stack underflow"
        | Multiply -> 
            match stack with
            | (x::y::rest) ->
                evaluateRPN' tl ((x * y)::rest)
            | _ -> failwith "stack underflow"
        | OpenGroup 
        | CloseGroup -> failwith "Convert to RPN first!"

let evaluateRPN tokens =
    evaluateRPN' tokens []
    |> List.head

let evaluate tokens =
    toRPN tokens
    |> evaluateRPN

let part1 expressions =
    List.sumBy evaluate expressions
    |> printfn "Part1: %d"


let rec toRPN2' tokens output stack =
    //printfn "------\ntokens: %A\noutput: %A\nstack: %A\n------" tokens output stack
    match tokens with
    | [] -> List.append (List.rev stack) output |> List.rev
    | (hd::tl) ->
        let (before, after) = splitAtOpen stack []
        match hd with
        | Number(x) -> toRPN2' tl ((Number(x))::output) stack
        | Add ->
            let (adds, nonAdds) = ((List.takeWhile (fun x -> x = Add) (List.rev before)), (List.skipWhile (fun x -> x = Add) (List.rev before))) 
            toRPN2' tl (List.append adds output) (Add::(List.append nonAdds after))
        | Multiply -> 
            toRPN2' tl (List.append before output) (Multiply::after)
        | OpenGroup -> toRPN2' tl output (OpenGroup::stack)
        | CloseGroup ->
            toRPN2' tl (List.append before output) (List.tail after)

// 1 + ( 2 * ( 3 + 4 ) + 5) -> 1 2 3 4 + * 5 +
let toRPN2 tokens =
    toRPN2' tokens [] []

let evaluateWPrecedence tokens = 
    toRPN2 tokens
    |> evaluateRPN

let part2 expressions =
    List.sumBy evaluateWPrecedence expressions
    |> printfn "Part2: %d"

let tokenized = getData filename |> tokenize

part1 tokenized
part2 tokenized