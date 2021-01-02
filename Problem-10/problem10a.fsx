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
let adaptersWithDevice = 0::connectedAdapters @ [ (List.max connectedAdapters) + 3 ]
 
let _,n1,n3 =
    adaptersWithDevice
    |> List.fold (fun (prev,numOf1,numOf3) conn ->
            match prev with
            | None -> (Some conn),numOf1,numOf3
            | Some p ->
                let diff = conn - p
                let numOf1' = if diff = 1 then numOf1+1 else numOf1
                let numOf3' = if diff = 3 then numOf3+1 else numOf3
                (Some conn),numOf1',numOf3'
        ) (None, 0, 0)

let result = n1 * n3
printfn "Result: %i" result

// 22 differences of 1 jolt and 10 differences of 3 jolts