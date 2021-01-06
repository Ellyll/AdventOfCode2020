open System

//let fileName = "Problem-13/problem13.test.data"
let fileName = "Problem-13/problem13.data"

let startTime, busIds =
    IO.File.ReadAllLines(fileName)
    |> function
        | [| start ; buses |] ->
            let sTime = Int64.Parse(start)
            let bIds =
                buses.Split(',')
                |> Array.filter (fun b -> b <> "x") // exclude not in service
                |> Array.map (Int64.Parse)
            (sTime,bIds)
        | _ -> failwith "Unable to parse file - must be 2 lines"

let busWaitTimes =
    busIds
    |> Array.map (fun bId ->
            let a = startTime/bId
            let b = a * bId
            if b = startTime then
                (bId, startTime)
            else
                (bId,(a+1L) * bId)
        )

let result =
    busWaitTimes
    |> Array.minBy (snd)
    |> fun (bId,time) ->
        bId * (time - startTime)

printfn "Result: %i" result

