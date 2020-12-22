//let fileName = "Problem-03/problem03.test.data"
let fileName = "Problem-03/problem03.data"

let world = System.IO.File.ReadAllLines(fileName)

let isTreeAt x y (world: string array) =
    let x' = x % (String.length world.[0])
    let y' = y % (Array.length world)
    world.[y'].[x'] = '#'

let rec countTrees x y totalTrees (world: string array) =
    if y >= Array.length world then
        totalTrees
    else
        let x' = x + 3
        let y' = y + 1
        let totalTrees' = totalTrees + (if isTreeAt x y world then 1 else 0)
        countTrees x' y' totalTrees' world

let result = world |> countTrees 0 0 0

printfn "Result: %i" result