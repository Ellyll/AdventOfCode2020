open System

//let fileName = "Problem-05/problem05.test.data"
let fileName = "Problem-05/problem05.data"

let lines = System.IO.File.ReadAllLines(fileName)

let getValue (str: string) minChar maxChar minRange maxRange =
    let rec loop chrs min max =
        match chrs with
        | [] -> min
        | c::rest ->
            match c with
            | x when x = minChar ->
                // 63 to 127, 63 to (63+(127-63)/2)
                let min' = min
                let max' = min + ((max-min)/2)
                loop rest min' max'
            | x when x = maxChar ->
                // 63 to 127, (63+(127-63)/2)+1 to 127
                let min' = min + ((max-min)/2) + 1
                let max' = max
                loop rest min' max'
            | x -> failwithf "Invalid character: %c" x
    loop (str.ToCharArray() |> List.ofArray) minRange maxRange


let getRow (str: string) =
    getValue str 'F' 'B' 0 127 

let getColumn (str: string) =
    getValue str 'L' 'R' 0 7

let passes =
    lines
    |> Array.map (fun line ->
        let row = getRow line.[0..6]
        let col = getColumn line.[7..9]
        let seatId = (row * 8) + col
        row, col, seatId
        )

let result = passes |> Array.map (fun (_,_,seatId) -> seatId) |> Array.max
printfn "Result: %i" result
