let fileName = "Problem-01/problem01a.data"
//let fileName = "Problem-01/problem01.test.data"

let data =
    System.IO.File.ReadAllLines(fileName)
    |> Array.map (fun s -> System.Int32.Parse(s))
    |> List.ofArray


let rec foldWithRemaining func state lst =
    match lst with
    | [] -> state
    | _ ->
        let newState = func state lst
        (List.tail lst) |> foldWithRemaining func newState


let rec findCombinations xs (n: int) =
    if n < 1 then
        failwithf "n must be >= 1 but is %i" n 
    elif n > List.length xs then
        []
    elif n = 1 then
        xs |> List.map (fun x -> [x])
    elif n = 2 then
        let head = xs |> List.head
        let tail = xs |> List.tail 
        (tail |> List.map (fun t -> [ head ; t ])) @ (findCombinations tail 2)
    else
        // [ a ; b ; c ; d ; e ; f ; ... ]
        // Combinations of n are a + cominations n-1 of b,c,d... @ b + combinations n-1 of c,d ... @ c + combinations n-1 of d,e ... etc
        xs
        |> foldWithRemaining (fun combinations remaining ->
                let head = remaining |> List.head
                let tail = remaining |> List.tail 
                ((findCombinations tail (n-1)) |> List.map (fun cs -> head::cs))
                @ combinations
            ) []

let find2020 data =
    findCombinations data 3
    |> List.tryFind (fun combinations ->
        match combinations with
        | [ a ; b; c ] -> a + b + c = 2020
        | _ -> failwithf "Invalid combination data: %A" combinations)

let (a,b,c) =
    match find2020 data with
    | None -> failwith "Unable to find 2020"
    | Some [a ; b ; c ] -> (a,b,c)
    | Some invalid -> failwithf "Invalid data: %A" invalid

let result = a * b * c

printfn "Result: %i" result
