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

let writeToMemory (location: int64) (value: int64) (mask: string) (memory: Map<int64,int64>) =
    let ones = Convert.ToInt64(mask.Replace("X", "0"), 2)
    let zeros = Convert.ToInt64(mask.Replace("1", "X").Replace("0", "1").Replace("X", "0"), 2)
    let maskedValue = ~~~((~~~(value ||| ones)) ||| zeros)
    // printfn "value:\t%s\t(decimal: %i)" ((sprintf "%36s" (Convert.ToString(value, 2))).Replace(" ", "0")) value
    // printfn "mask:\t%s" mask
    // printfn "result:\t%s\t(decimal: %i)\n" ((sprintf "%36s" (Convert.ToString(maskedValue, 2))).Replace(" ", "0")) maskedValue
    if memory |> Map.containsKey location then
        memory |> Map.remove location |> Map.add location maskedValue
    else
        memory |> Map.add location maskedValue

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
