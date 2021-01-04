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
        X: int
        Y: int
        Facing: Facing
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

let runCommand command shipState =
    match command with
    | GoNorth n -> { shipState with Y = shipState.Y + n }
    | GoEast n -> { shipState with X = shipState.X + n }
    | GoSouth n -> { shipState with Y = shipState.Y - n }
    | GoWest n -> { shipState with X = shipState.X - n }
    | TurnRight degrees -> { shipState with Facing = turn degrees shipState.Facing }
    | TurnLeft degrees -> { shipState with Facing = turn -degrees shipState.Facing }
    | GoForward r ->
        match shipState.Facing with
        | North -> { shipState with Y = shipState.Y + r }
        | East ->  { shipState with X = shipState.X + r }
        | South -> { shipState with Y = shipState.Y - r }
        | West ->  { shipState with X = shipState.X - r }

let getManhattanDistance (px,py) (qx,qy) =
    // the taxicab distance between ( p 1 , p 2 ) and ( q 1 , q 2 ) is | p 1 − q 1 | + | p 2 − q 2 |
    (abs (px - qx)) + (abs (py - qy))

let commands =
    IO.File.ReadAllLines(fileName)
    //|> Array.take 10
    |> Array.map parseCommand

let finalState =
    commands
    |> Seq.fold (fun shipState command ->
        let shipState' = runCommand command shipState
        //printfn "After %A,\tshipState': %A" command shipState'
        shipState'
        ) { X = 0 ; Y = 0 ; Facing = East }

printfn "finalState: %A" finalState

let result = getManhattanDistance (0,0) (finalState.X,finalState.Y)
printfn "Result: %i" result