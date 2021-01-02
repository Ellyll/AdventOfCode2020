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


let getNextGenerationOfSeat x y (grid: char [,]) =
    let current = grid.[x,y]
    if current = '#' || current = 'L' then
        let adjacents =
            [
                (-1,1)  ; (0,1)  ; (1,1)
                (-1,0)  ;          (1,0)
                (-1,-1) ; (0,-1) ; (1,-1)
            ]
            |> List.map (fun (dx,dy) -> (x+dx,y+dy))
        
        let numberOccupied =
            adjacents
            |> List.fold (fun acc (x',y') ->
                if (x' >= 0 && x' < Array2D.length1 grid) &&
                   (y' >= 0 && y' < Array2D.length2 grid) &&
                   grid.[x',y'] = '#' then
                    acc + 1
                else
                    acc
                ) 0
        (*
        - If a seat is empty (L) and there are no occupied seats adjacent to it, the seat becomes occupied.
        - If a seat is occupied (#) and four or more seats adjacent to it are also occupied, the seat becomes empty.
        - Otherwise, the seat's state does not change.
        *)
        if current = 'L' && numberOccupied = 0 then
            '#'
        elif current = '#' && numberOccupied >= 4 then
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


// printfn "Grid 1:"
// printGrid grid

// printfn "\nFinal grid:"
// printGrid finalGrid

printfn "Result: %i" result