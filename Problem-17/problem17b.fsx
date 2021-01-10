//let fileName = "Problem-17/problem17.test.data"
let fileName = "Problem-17/problem17.data"

let getInitialGrid () =
    let tmp =
        System.IO.File.ReadAllLines(fileName)
        |> Array.map (fun line -> line.ToCharArray())
    seq {
        for y in 0..((Array.length tmp)-1) do
            for x in 0..((Array.length tmp.[0])-1) do
                if (tmp.[y].[x] = '#') then
                    yield (x,y,0,0)
    }
    |> Set.ofSeq

let getLocationsOfNeighbours (x,y,z,w) =
    [
        for dw in -1..1 do
            for dz in -1..1 do
                for dy in -1..1 do
                    for dx in -1..1 do
                        if not (dx = 0 && dy = 0 && dz = 0 && dw = 0) then
                            yield (dx,dy,dz,dw)
    ]
    |> List.map (fun (dx,dy,dz,dw) -> (x+dx,y+dy,z+dz,w+dw))
    |> Set.ofList


let getNextCycle grid =
    let locationsToConsider =
        grid
        |> Set.fold (fun locations (x,y,z,w) ->
            locations + getLocationsOfNeighbours (x,y,z,w)
            ) Set.empty

    locationsToConsider
    |> Set.fold (fun newGrid location ->
        (*
        - If a cube is active and exactly 2 or 3 of its neighbors are also active, the cube remains active. Otherwise, the cube becomes inactive.
        - If a cube is inactive but exactly 3 of its neighbors are active, the cube becomes active. Otherwise, the cube remains inactive.
        *)
        let neighbours = getLocationsOfNeighbours location
        let numOfActive =
            Set.intersect neighbours grid
            |> Set.count
        let isActive = grid |> Set.contains location
        if isActive then
            if numOfActive >= 2 && numOfActive <= 3 then
                newGrid |> Set.add location
            else
                newGrid
        else
            if numOfActive = 3 then
                newGrid |> Set.add location
            else
                newGrid

        ) Set.empty

let initialGrid = getInitialGrid ()

let finalGrid =
    [ 1..6 ]
    |> List.fold (fun grid _ ->
        getNextCycle grid
        ) initialGrid

let result = finalGrid |> Set.count
printfn "Result: %i" result