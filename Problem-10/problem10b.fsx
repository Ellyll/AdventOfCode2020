open System
open System.IO

//let fileName = "Problem-10/problem10.test.data"
let fileName = "Problem-10/problem10.data"

let adapters =
    File.ReadAllLines(fileName)
    |> Array.map (fun line -> Int32.Parse(line))
    |> Array.sort
    |> ResizeArray

let findCompatibleConnectorIndex n connectors =
    connectors
    |> Seq.tryFindIndex (fun x -> x - n >= 0 && x - n <=3 )
    |> function
        | Some idx -> idx
        | None -> failwithf "Unable to find connector for %i" n

let rec connectAdapters (connected: int list) (remaining: ResizeArray<int>) =
    if remaining.Count = 0 then
        connected |> List.rev
    else
        let target = if List.isEmpty connected then 0 else List.head connected
        let newConnectedIndex = findCompatibleConnectorIndex target remaining
        let newConnected = remaining.[newConnectedIndex]
        remaining.RemoveAt(newConnectedIndex)
        connectAdapters (newConnected::connected) remaining


let connectedAdapters = (connectAdapters [] adapters)


let getPossibilities adapters : int64 =
    let rec loop (from: int) (cache: Map<int,int64>): int64 * Map<int,int64> =
        if cache |> Map.containsKey from then
            let value = (cache |> Map.find from)
            value, cache
        else
            let nextPossibleValues = adapters |> Set.filter (fun n -> n > from && n <= (from+3))
            if nextPossibleValues |> Set.isEmpty then
                let newPossibilities = 1L
                let newCache = cache |> Map.add from newPossibilities
                newPossibilities,newCache
            else
                let newPossibilities, newCache =
                    nextPossibleValues
                    |> Set.fold (fun (possibilities,cache') n ->
                        let p, c = loop n cache'
                        ((possibilities + p), c)
                        ) (0L,cache)
                    |> fun (possibilities, cache') -> possibilities,cache'
                newPossibilities, newCache |> Map.add from newPossibilities
    loop 0 Map.empty |> fst

let possibilities = getPossibilities (Set.ofList connectedAdapters)

let result = possibilities
printfn "Result: %i" result
