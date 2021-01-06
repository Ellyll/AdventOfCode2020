open System

//let fileName = "Problem-13/problem13.test.data"
let fileName = "Problem-13/problem13.data"

let constraints =
    IO.File.ReadAllLines(fileName)
    |> function
        | [| _ ; buses |] ->
            buses.Split(',')
            |> Array.mapi (fun i x -> (int64 i,x))
            |> Array.filter (fun (_,x) -> x <> "x")
            |> Array.map (fun (i,bId) -> (i,(Int64.Parse(bId))))
            |> Array.toList
        | _ -> failwith "Unable to parse file - must be 2 lines"

let getNextArrivalTime time bId =
        let a = time/bId
        let b = a * bId
        if b = time then
            time
        else
            (a+1L) * bId

let areConstraintsMet t constraints =
    constraints
    |> Seq.forall (fun (i,bId) ->
        let arrivalTime = getNextArrivalTime (t+i) bId
        arrivalTime = (t + i)
        )

let findNextValue i bId t step =
    let rec loop t' =
        if (t'+i) % bId = 0L then
            t'
        else
            loop (t'+step)
    loop t

// Thanks very much to PillarsBliz in their reply to https://www.reddit.com/r/adventofcode/comments/kc60ri/2020_day_13_can_anyone_give_me_a_hint_for_part_2/
// Spoiler: https://paste.debian.net/plainh/f26a33ae
let findResult (constraints: (int64*int64) list) =
    constraints
    |> List.fold (fun (t,step) (i,bId) ->
        let newT =
            if t = 0L then
                bId
            else
                // find where t+i is disible wholey by bId, add step until we find it
                findNextValue i bId t step
        let newStep = step * bId
        (newT,newStep)
        ) (0L,1L)
    |> fst

let result = findResult constraints

printfn "Result: %i" result
