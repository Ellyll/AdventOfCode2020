open System
open System.IO

//let fileName, preambleLength = "Problem-09/problem09.test.data", 5
let fileName, preambleLength = "Problem-09/problem09.data", 25

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


let values =
    File.ReadAllLines(fileName)
    |> Array.map (fun line -> Int64.Parse(line))

let sumsOfPairs previousValues =
    findCombinations previousValues 2
    |> List.fold (fun state pair ->
        match pair with
        | [ a ; b ] ->
            let k = a + b
            let v = (a, b)
            match state |> Map.tryFind k with
            | Some vOld -> state |> Map.remove k |> Map.add k (v::vOld)
            | None -> state |> Map.add k ([ v ])
        | _ -> failwithf "Invalid pair: %A" pair
        ) Map.empty

let findi (predicate: int -> 'a -> bool) (arr: 'a array) =
    let rec loop idx =
        if predicate idx arr.[idx] then
            arr.[idx]
        else
            loop (idx+1)
    loop 0

let tryFindi (predicate: int -> 'a -> bool) (arr: 'a array) =
    let rec loop idx =
        if idx >= Array.length arr then
            None
        elif predicate idx arr.[idx] then
            Some arr.[idx]
        else
            loop (idx+1)
    loop 0


let tryFindContiguousSum target (arr: int64 array) =
    let rec subLoop idx lst =
        if idx >= Array.length arr then
            None
        else
            let newLst = arr.[idx]::lst
            let sum = List.sum(newLst)
            if sum = target then
                Some lst
            elif sum > target then
                None
            else
                subLoop (idx+1) newLst

    let rec mainLoop idx =
        if idx >= Array.length arr then
            None
        else
            match subLoop idx [] with
            | Some lst -> Some lst
            | None -> mainLoop (idx+1)
                    
    mainLoop 0

    

let target =
    values
    |> findi (fun i n ->
        i >= preambleLength && not (
            Array.sub values (i-preambleLength) preambleLength
            |> Array.toList
            |> sumsOfPairs
            |> Map.containsKey n ))

let result =
    tryFindContiguousSum target values
    |> function
        | Some xs -> (List.min xs) + (List.max xs)
        | None -> failwith "Unable to find contiguous sum"

printfn "Result: %i" result

