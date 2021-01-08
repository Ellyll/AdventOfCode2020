let start =
    //[ 3 ; 1 ; 2]
    [ 13 ; 0 ; 10 ; 12 ; 1 ; 5 ; 8 ]
    |> List.mapi (fun i x -> x,(i+1))
    |> Map.ofList

let getNthNumber n history =
    if history |> Map.containsKey n then
        history |> Map.find n
    else
        let rec getNext limit prevTurnNumber prevNumber hist =
            //printfn "limit: %i prevTurnNumber: %i prevNumber: %i hist: %A" limit prevTurnNumber prevNumber hist
            let thisTurnNumber = prevTurnNumber + 1
            (*
            If that was the first time the number has been spoken, the current player says 0.
            Otherwise, the number had been spoken before; the current player announces how many turns apart the number is from when it was previously spoken.
            *)
            let thisNumber =
                if hist |> Map.containsKey prevNumber then
                    let prevSpokenOn = hist |> Map.find prevNumber
                    prevTurnNumber - prevSpokenOn
                else
                    0
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


let result = getNthNumber 2020 start

printfn "Result: %i" result