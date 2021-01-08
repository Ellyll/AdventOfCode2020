open System

//let fileName = "Problem-14/problem14.test.data"
let fileName = "Problem-14/problem14.data"

type Instruction =
    | SetMask of string
    | WriteToMemory of (int64*int64)

let parseInstruction (line: string) =
    if line.StartsWith("mask") then
        let mask = line.Split('=').[1].Trim()
        SetMask mask
    elif line.StartsWith("mem") then
        let parts = line.Split('=') |> Array.map (fun s -> s.Trim())
        let location = parts.[0].Replace("mem[", "").Replace("]", "") |> Int64.Parse
        let value = parts.[1] |> Int64.Parse
        WriteToMemory (location,value)
    else
        failwithf "Unable to parse line: %s" line

let getPossibleLocations (maskedLocation: string) =
    let numberOfXs = maskedLocation.ToCharArray() |> Array.filter (fun x -> x = 'X') |> Array.length
    if numberOfXs = 0 then
        seq { (Convert.ToInt64(maskedLocation,2)) }
    else
        let start = 0L
        let finish = (Convert.ToInt64(String.replicate numberOfXs "1", 2))
        seq {
            for n in start..finish do
                let binaryStr = Convert.ToString(n, 2).PadLeft(numberOfXs, '0')
                let location =
                    maskedLocation.ToCharArray()
                    |> Array.fold (fun (str,remaining) c ->
                        if c = '0' then (str + "0",remaining)
                        elif c = '1' then (str + "1", remaining)
                        elif c = 'X' then (str + ((List.head remaining).ToString()), List.tail remaining)
                        else failwithf "Invalid character in maskedLocation: %c" c
                    ) ("",(binaryStr.ToCharArray() |> Array.toList))
                    |> fst
                    |> fun s -> Convert.ToInt64(s, 2)
                yield location
        }

let writeToMemory (location: int64) (value: int64) (mask: string) (memory: Map<int64,int64>) =
    let locationStr = ((sprintf "%36s" (Convert.ToString(location, 2))).Replace(" ", "0"))
    let maskedLocation =
        locationStr.ToCharArray()
        |> Seq.zip (mask.ToCharArray())
        |> Seq.map (fun (m,l) ->
            (*
                If the bitmask bit is 0, the corresponding memory address bit is unchanged.
                If the bitmask bit is 1, the corresponding memory address bit is overwritten with 1.
                If the bitmask bit is X, the corresponding memory address bit is floating.
            *)
            if m = '0' then l
            elif m = '1' then '1'
            elif m = 'X' then 'X'
            else failwithf "Invalid bitmask character: %c" m
            )
        |> Seq.toArray
        |> fun arr -> String(arr)
    let possibleLocations = getPossibleLocations maskedLocation
    possibleLocations
    |> Seq.fold (fun memory' loc ->
        if memory' |> Map.containsKey loc then
            memory' |> Map.remove loc |> Map.add loc value
        else
            memory' |> Map.add loc value
    ) memory


let instructions =
    IO.File.ReadAllLines(fileName)
    |> Array.map parseInstruction

let memoryAfterRun, maskAfterRun =
    instructions
    |> Array.fold (fun (memory,mask) instruction ->
        match instruction with
        | SetMask m -> (memory,m)
        | WriteToMemory (location,value) ->
            let memory' = writeToMemory location value mask memory
            (memory',mask)
    ) (Map.empty,"XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX")

let result =
    memoryAfterRun
    |> Map.toList
    |> List.sumBy(snd)

printfn "Result: %i" result
