//let fileName = "Problem-03/problem03.test.data"
let fileName = "Problem-03/problem03.data"

let world = System.IO.File.ReadAllLines(fileName)

let isTreeAt x y (world: string array) =
    let x' = x % (String.length world.[0])
    let y' = y % (Array.length world)
    world.[y'].[x'] = '#'

let rec countTrees x y totalTrees dx dy (world: string array) =
    if y >= Array.length world then
        totalTrees
    else
        let x' = x + dx
        let y' = y + dy
        let totalTrees' = totalTrees + (if isTreeAt x y world then 1 else 0)
        countTrees x' y' totalTrees' dx dy world


let routes =
    [
        1, 1
        3, 1
        5, 1
        7, 1
        1, 2
    ]

let numberOfTrees =
    routes
    |> List.map (fun (dx,dy) -> world |> countTrees 0 0 0 dx dy |> int64)


let result =
    numberOfTrees
    |> List.fold (( * )) 1L


printfn "Result: %i" result