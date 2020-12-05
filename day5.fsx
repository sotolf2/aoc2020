let filename = "day5.txt"

let getData filename = 
    System.IO.File.ReadAllLines filename 
    |> List.ofSeq

let lower sq =
    List.take ((List.length sq) / 2) sq

let upper sq =
    List.skip ((List.length sq) / 2) sq

let rowTrans = function
| 'B' -> upper
| 'F' -> lower
| c -> failwith (sprintf "unexpected character %c" c)

let colTrans = function
| 'R' -> upper
| 'L' -> lower
| c -> failwith (sprintf "unexpected character %c" c)

let findRow (str : string) =
    let searchStr = str.Substring(0, 7) |> List.ofSeq |> List.map rowTrans
    List.fold (fun st f -> f st) [0..127] searchStr
    |> List.head

let findCol (str : string) =
    let searchStr = str.Substring(7,3) |> List.ofSeq |> List.map colTrans
    List.fold (fun st f -> f st) [0..7] searchStr
    |> List.head
    
let findSeat bsp =
    (findRow bsp, findCol bsp)

let seatId (row, col) =
    row * 8 + col

let part1 bps =
    (List.map (findSeat >> seatId) bps)
    |> List.reduce max
    |> printfn "Part1: %d"

let boardingpasses = getData filename 


//part1 
//part2