let filename = "day11.txt"

type State = Floor | Seat | Occupied

let getData filename = 
    System.IO.File.ReadAllLines filename 
    |> List.ofSeq
    |> List.map List.ofSeq

let trans = function
| 'L' -> Seat
| '.' -> Floor
| '#' -> Occupied
| ch -> failwithf "Unexpected character %c" ch

let parse lns =
    let width = List.length (List.head lns)
    let heigth = (List.length lns)

    let getState row col =
        trans lns.[row].[col]
    
    Array2D.init heigth width getState

let neighbourCoords row col maxrow maxcol =
    let sugg = [(row - 1, col - 1); (row - 1, col);  (row - 1, col + 1);
                (row    , col - 1);                  (row    , col + 1);
                (row + 1, col - 1); (row + 1, col);  (row + 1, col + 1)]
    
    List.filter (fun (row, col) -> row >= 0 && col >=0) sugg
    |> List.filter (fun (row, col) -> row < maxrow && col < maxcol)

let nextstateof grid row col curstate =
    let neighbourstates = neighbourCoords row col (Array2D.length1 grid) (Array2D.length2 grid)
                        |> List.map (fun (row, col) -> grid.[row,col])
                        |> List.countBy id
                        |> List.filter (fun (id, _) -> id = Occupied)
                        |> List.tryHead
                        |> Option.map snd

    match (curstate, neighbourstates) with
    | (Seat, None) -> Occupied
    | (Occupied, Some(n)) when n >= 4 -> Seat
    | (state, _) -> state
    

let next states =
    let nextstate = nextstateof states
    Array2D.mapi nextstate states

let rec getStable state =
    let nexts = next state
    if nexts = state then state else
    getStable nexts

let countOccupied state =
    [0..(Array2D.length1 state) - 1]
    |> List.sumBy (fun x -> state.[x,*] 
                           |> Array.filter (fun x -> x = Occupied)
                           |> Array.length)

let part1 state =
    getStable state
    |> countOccupied
    |> printfn "Part1: %d"     

let startstate = getData filename |> parse
