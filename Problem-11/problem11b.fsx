//let fileName = "Problem-11/problem11.test.data"
let fileName = "Problem-11/problem11.data"

let grid =
    let tmp =
        System.IO.File.ReadAllLines(fileName)
        |> Array.map (fun line -> line.ToCharArray())
    Array2D.init (Array.length <| tmp.[0]) (Array.length tmp) (fun x y -> tmp.[y].[x])

let printGrid grid =
    for y in 0..(grid |> Array2D.length2)-1 do
        for x in 0..(grid |> Array2D.length2)-1 do
            printf "%c" grid.[x,y]
        printfn ""

let rec getSeatInDirection x y dx dy grid =
    let x' = x + dx
    let y' = y + dy
    if (x' >= 0 && x' < Array2D.length1 grid) &&
       (y' >= 0 && y' < Array2D.length2 grid) then
        let current = grid.[x',y']
        if current = '#' || current = 'L' then
            Some current
        else
            getSeatInDirection x' y' dx dy grid
    else
        None
    

let getNextGenerationOfSeat x y (grid: char [,]) =
    let current = grid.[x,y]
    if current = '#' || current = 'L' then
        let visibleSeats =
            [
                (-1,1)  ; (0,1)  ; (1,1)
                (-1,0)  ;          (1,0)
                (-1,-1) ; (0,-1) ; (1,-1)
            ]
            |> List.map (fun (dx,dy) -> getSeatInDirection x y dx dy grid)
        
        let numberOccupied =
            visibleSeats
            |> List.sumBy (fun seat ->
                match seat with
                | Some '#' -> 1
                | _ -> 0
                )

        (*
        - If a seat is empty (L) and there are no occupied seats visible to it, the seat becomes occupied.
        - If a seat is occupied (#) and five or more seats visible to it are also occupied, the seat becomes empty.
        - Otherwise, the seat's state does not change.
        *)
        if current = 'L' && numberOccupied = 0 then
            '#'
        elif current = '#' && numberOccupied >= 5 then
            'L'
        else
            current
    else
        current

let nextGenerationOfGrid grid =
    grid |> Array2D.mapi (fun x y _ -> getNextGenerationOfSeat x y grid)


let rec runUntilStable grid =
    // printfn "\nGrid:"
    // printGrid grid
    let negGen = nextGenerationOfGrid grid
    if negGen = grid then
        negGen
    else
        runUntilStable negGen


let finalGrid = runUntilStable grid

let result =
    let mutable count = 0
    finalGrid
    |> Array2D.iter (fun c -> if c = '#' then count <- count + 1)
    count

printfn "Result: %i" result