let filename = "day2.txt"

type Rule = {
            Char: char;
            Min: int;
            Max: int;
}

let getData filename = 
    System.IO.File.ReadAllLines filename 
    |> List.ofSeq

let parseLine (line: string) =
    let parts = line.Split ':'
    let rule = parts.[0]
    let rules = rule.Split([|'-'; ' '|])
    let passw = parts.[1]
    ({ Char = rules.[2].[0];
       Min = int rules.[0];
       Max = int rules.[1];
    }, passw)

let parse lines =
    List.map parseLine lines

let checkPasswd (rule, passw) =
    String.filter (fun ch -> ch = rule.Char) passw
    |> String.length
    |> (fun x -> x >= rule.Min && x <= rule.Max)

let part1 lines =
    let correct =
        lines
        |> List.filter checkPasswd
        |> List.length
    
    printfn "Part1: %d" correct

let checkPasswd2 ((rule, passw): Rule * string) =
    let pos1 = passw.[rule.Min]
    let pos2 = passw.[rule.Max]
    (pos1 = rule.Char) <> (pos2 = rule.Char)

let part2 lines =
    let correct =
        lines
        |> List.filter checkPasswd2
        |> List.length

    printfn "Part2: %d" correct

let lines = getData filename |> parse

part1 lines
part2 lines

// let test = ["1-3 a: abcde";"1-3 b: cdefg";"2-9 c: ccccccccc"]
