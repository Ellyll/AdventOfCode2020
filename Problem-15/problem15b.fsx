let start =
    //[ 3L ; 1L ; 2L ]
    [ 13L ; 0L ; 10L ; 12L ; 1L ; 5L ; 8L ]
    |> List.mapi (fun i x -> x, int64 (i+1))
    |> Map.ofList

let getNthNumber n history =
    if history |> Map.containsKey n then
        history |> Map.find n
    else
        let rec getNext limit prevTurnNumber prevNumber hist =
            if prevTurnNumber % 1_000_000L = 0L then
                printfn "limit: %i prevTurnNumber: %i prevNumber: %i hist: %A" limit prevTurnNumber prevNumber hist
            let thisTurnNumber = prevTurnNumber + 1L
            (*
            If that was the first time the number has been spoken, the current player says 0.
            Otherwise, the number had been spoken before; the current player announces how many turns apart the number is from when it was previously spoken.
            *)
            let thisNumber =
                if hist |> Map.containsKey prevNumber then
                    let prevSpokenOn = hist |> Map.find prevNumber
                    prevTurnNumber - prevSpokenOn
                else
                    0L
            let hist' =
                if hist |> Map.containsKey prevNumber then
                    hist |> Map.remove prevNumber
                else
                    hist
                |> Map.add prevNumber prevTurnNumber
            if thisTurnNumber = limit then
                thisNumber
            else
                getNext limit thisTurnNumber thisNumber hist'
        let startNumber, startTurnNumber = history |> Map.toList |> List.maxBy (snd)
        getNext n startTurnNumber startNumber history


let result = getNthNumber 30000000L start

printfn "Result: %i" result