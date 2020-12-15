let starting = [1UL;20UL;11UL;6UL;12UL;0UL];;

//let starting = [0;3;6]

let rec count' cur prev goal map =
    //printfn "--count--"
    //printfn "cur: %d prev: %d map: %A" cur prev map
    if cur = (goal + 1UL) then prev else
    if Map.containsKey prev map then
        count' (cur + 1UL) ((cur - 1UL) - (Map.find prev map)) goal (Map.add prev (cur - 1UL) map)
    else
        count' (cur + 1UL) 0UL goal (Map.add prev (cur - 1UL) map)


let count lst goal =
    let startingMap = List.mapi (fun i elm -> (elm, ((uint64 i) + 1UL))) lst |> Map.ofList
    let n = List.length lst |> uint64

    count' (n + 1UL) (List.last lst) goal startingMap

printfn "Part1: %d" (count starting 2020UL)
printfn "Part2: %d" (count starting 30000000UL)