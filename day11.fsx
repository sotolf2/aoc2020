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
    let cols = List.length (List.head lns)
    let rows = (List.length lns)

    let getState row col =
        trans lns.[row].[col]
    
    Array2D.init rows cols getState

let neighbourCoords row col maxrow maxcol =
    let sugg = [(row - 1, col - 1); (row - 1, col);  (row - 1, col + 1);
                (row    , col - 1);                  (row    , col + 1);
                (row + 1, col - 1); (row + 1, col);  (row + 1, col + 1)]
    
    List.filter (fun (row, col) -> row >= 0 && col >=0) sugg
    |> List.filter (fun (row, col) -> row < maxrow && col < maxcol)

let Neighbour grid row col curstate =
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
    
let rec path row col maxrow maxcol (vrow, vcol) acc =
    let nrow = row + vrow
    let ncol = col + vcol
    if nrow >= maxrow || nrow < 0 then List.rev acc else
    if ncol >= maxcol || ncol < 0 then List.rev acc else
    path nrow ncol maxrow maxcol (vrow, vcol) ((nrow, ncol)::acc)

let seencoord row col maxrow maxcol =
    //              - NW -    - N -    - NE-     - W -    - E -    - SW -    - S -     - SE -
    let vectors = [(-1, -1); (-1, 0); (-1, +1); (0, -1); (0, +1); (+1, -1); (+1, 0); (+1, +1)]
    List.map (fun x -> path row col maxrow maxcol x []) vectors

let containsOccupied coords (grid: State [,]) =
    let firstnonfloor = List.map (fun (row, col) -> grid.[row,col]) coords
                        |> List.skipWhile (fun x -> x = Floor)
                        |> List.tryHead
    match firstnonfloor with
    | None -> false
    | Some(Seat) -> false
    | Some(Occupied) -> true
    | Some(Floor) -> failwith "First nonefloor is floor"

let LOS grid row col curstate =
    let linesOfSight =  seencoord row col (Array2D.length1 grid) (Array2D.length2 grid)
                        |> List.map (fun coords -> containsOccupied coords grid)
                        |> List.filter id
                        |> List.length

    match (curstate, linesOfSight) with
    | (Seat, 0) -> Occupied
    | (Occupied, n) when n >= 5 -> Seat
    | (state, _) -> state

let next states method =
    let nextstate = method states
    Array2D.mapi nextstate states

let rec getStable state method =
    let nexts = next state method
    if nexts = state then state else
    getStable nexts method

let countOccupied state =
    [0..(Array2D.length1 state) - 1]
    |> List.sumBy (fun x -> state.[x,*] 
                           |> Array.filter (fun x -> x = Occupied)
                           |> Array.length)

let part1 state =
    getStable state Neighbour
    |> countOccupied
    |> printfn "Part1: %d"     

let part2 state =
    getStable state LOS
    |> countOccupied
    |> printfn "Part2: %d"

let startstate = getData filename |> parse

part1 startstate
part2 startstate
