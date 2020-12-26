open System
open System.IO

type Instruction =
    | Acc of int64
    | Nop of int64
    | Jmp of int64

type SystemState =
    {
        InstructionPtr: int64
        Instructions: Map<int64,Instruction>
        ExecutionCounts: Map<int64,int>
        Accumulator: int64
    }

type ExecutionResult =
    | InfiniteLoop
    | OutOfBounds of int64

//let fileName = "Problem-08/problem08.test.data"
let fileName = "Problem-08/problem08.data"

let parseInstruction (line: string) : Instruction =
    let parts = line.Split(' ')
    match parts.[0] with
    | "acc" -> Acc (Int64.Parse(parts.[1]))
    | "nop" -> Nop (Int64.Parse(parts.[1]))
    | "jmp" -> Jmp (Int64.Parse(parts.[1]))
    | _ -> failwithf "Unable to parse instruction from \"%s\"" line

let loadState fileName =
    File.ReadAllLines(fileName)
    |> Seq.fold (fun (state, idx) line ->
        let instruction = parseInstruction line
        { state with InstructionPtr = 0L ; Instructions = state.Instructions |> Map.add idx instruction ; ExecutionCounts = state.ExecutionCounts |> Map.add idx 0 }, (idx+1L)
        ) ({ InstructionPtr = 0L ; Instructions = Map.empty ; ExecutionCounts = Map.empty ; Accumulator = 0L },0L)
    |> fst

let rec execute state =
    match state.Instructions |> Map.tryFind state.InstructionPtr with
    | None -> OutOfBounds state.Accumulator
    | Some instruction ->
        let executionCount = state.ExecutionCounts |> Map.find state.InstructionPtr
        if executionCount >= 1 then
            InfiniteLoop
        else
            let newExecutionCounts =
                state.ExecutionCounts
                |> Map.change state.InstructionPtr (fun x -> x |> Option.map (fun n -> (n+1)))
            let state' = { state with ExecutionCounts = newExecutionCounts }
            let newState =
                match instruction with
                | Acc n -> { state' with Accumulator = state'.Accumulator + n ; InstructionPtr = state'.InstructionPtr + 1L }
                | Nop _ -> { state' with InstructionPtr = state'.InstructionPtr + 1L }
                | Jmp n -> { state' with InstructionPtr = state'.InstructionPtr + n }
            execute newState

let initalState = loadState fileName

let result =
    initalState.Instructions
    |> Map.toSeq
    |> Seq.filter (fun (_,inst) ->
        match inst with
        | Jmp _ -> true
        | Nop _ -> true
        | _ -> false)
    |> Seq.pick (fun (idx,inst) ->
        let newInstruction =
            match inst with
            | Nop n -> Jmp n
            | Jmp n -> Nop n
            | i -> failwithf "Invalid instruction: %A" i
        let newInstructions =
            initalState.Instructions
            |> Map.change idx (Option.map (fun _ -> newInstruction))
        let executionResult = execute { initalState with Instructions = newInstructions}
        match executionResult with
        | InfiniteLoop -> None
        | OutOfBounds n -> Some n
        )

printfn "Result: %i" result
