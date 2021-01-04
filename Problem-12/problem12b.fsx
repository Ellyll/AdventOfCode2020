open System

//let fileName = "Problem-12/problem12.test.data"
let fileName = "Problem-12/problem12.data"

type Command =
    | GoNorth of int
    | GoEast of int
    | GoSouth of int
    | GoWest of int
    | TurnRight of int
    | TurnLeft of int
    | GoForward of int

type Facing =
    | North
    | East
    | South
    | West

type ShipState =
    {
        ShipX: int
        ShipY: int
        WaypointX: int
        WaypointY: int
    }

let parseCommand (line: string) =
    let amount = Int32.Parse(line.[1..])
    match line.[0] with
    | 'N' -> GoNorth amount
    | 'E' -> GoEast amount
    | 'S' -> GoSouth amount
    | 'W' -> GoWest amount
    | 'R' -> TurnRight amount
    | 'L' -> TurnLeft amount
    | 'F' -> GoForward amount
    | _ -> failwithf "Unable to parse: %s" line

let turnLeft f =
    match f with
    | North -> West
    | East -> North
    | South -> East
    | West -> South

let turnRight f =
    match f with
    | North -> East
    | East -> South
    | South -> West
    | West -> North

let turn degrees fromFacing =
    let steps = degrees / 90
    let rec loop s f =
        if s = 0 then
            f
        else
            if s > 0 then
                loop (s-1) (turnRight f)
            else
                loop (s+1) (turnLeft f)
    loop steps fromFacing

let getManhattanDistance (px,py) (qx,qy) =
    (abs (px - qx)) + (abs (py - qy))

let rotateWaypoint degrees shipState =
    if not <| List.contains (abs degrees) [ 90 ; 180 ; 270 ] then
        failwithf "Invalid degrees to rotate: %i" degrees
    let times =
        let n = degrees / 90
        if n < 0 then
            4 + n // e.g. turning left once is the same as turning right 3 times
        else
            n
    let newX, newY =
        match times with
        | 1 -> (shipState.WaypointY, -shipState.WaypointX)
        | 2 -> (-shipState.WaypointX, -shipState.WaypointY)
        | 3 -> (-shipState.WaypointY, shipState.WaypointX)
        | _ -> failwithf "Invalid value for times: %i" times
    { shipState with WaypointX = newX ; WaypointY = newY }

let goToWaypoint times shipState =
    // Waypoint is relative to ship
    let newSx = shipState.ShipX + (shipState.WaypointX*times)
    let newSy = shipState.ShipY + (shipState.WaypointY*times)
    { shipState with ShipX = newSx ; ShipY = newSy }

let runCommand command shipState =
    match command with
    | GoNorth n -> { shipState with WaypointY = shipState.WaypointY + n }
    | GoEast n -> { shipState with WaypointX = shipState.WaypointX + n }
    | GoSouth n -> { shipState with WaypointY = shipState.WaypointY - n }
    | GoWest n -> { shipState with WaypointX = shipState.WaypointX - n }
    | TurnRight degrees -> shipState |> rotateWaypoint degrees
    | TurnLeft degrees -> shipState |> rotateWaypoint -degrees
    | GoForward units -> shipState |> goToWaypoint units

let sprintShipState shipState =
    sprintf "{ ShipX = %i ShipY = %i WaypointX = %i WaypointY = %i }"
        shipState.ShipX
        shipState.ShipY
        shipState.WaypointX
        shipState.WaypointY


let commands =
    IO.File.ReadAllLines(fileName)
    |> Array.map parseCommand

let finalState =
    commands
    |> Seq.fold (fun shipState command ->
        let shipState' = runCommand command shipState
        //printfn "After %A,\tshipState': %s" command (sprintShipState shipState')
        shipState'
        ) { ShipX = 0 ; ShipY = 0 ; WaypointX = 10 ; WaypointY = 1 }

printfn "finalState: %A" finalState

let result = getManhattanDistance (0,0) (finalState.ShipX,finalState.ShipY)
printfn "Result: %i" result