
let data =
    System.IO.File.ReadAllLines("Problem-01/problem01a.data")
    |> Array.map (fun s -> System.Int32.Parse(s))
    |> List.ofArray

let rec find2020 remaining =
    match remaining with
    | [] -> None
    | [_] -> None
    | x::xs ->
        match (xs |> List.tryFind (fun y -> x + y = 2020)) with
        | Some y -> Some (x, y)
        | None -> find2020 xs

let a,b =
    match find2020 data with
    | None -> failwith "Unable to find 2020"
    | Some (x,y) -> x,y

let result = a * b

printfn "Result: %i" result
