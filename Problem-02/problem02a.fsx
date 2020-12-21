open System.Text.RegularExpressions

type Policy =
    {
        Min: int
        Max: int
        Letter: char
    }

//let fileName = "Problem-02/problem02.test.data"
let fileName = "Problem-02/problem02.data"

let regPolicy = 
    Regex("^(\d+)-(\d+) (.).*$")

let parsePolicy str =
    if not <| regPolicy.IsMatch(str) then
        failwithf "Invalid policy string: %s" str
    {
        Min = System.Int32.Parse(regPolicy.Replace(str, "$1"))
        Max = System.Int32.Parse(regPolicy.Replace(str, "$2"))
        Letter = regPolicy.Replace(str, "$3").ToCharArray().[0]
    }

let isPasswordValid policy (password: string) =
    let letterCounts =
        password.ToCharArray()
        |> Array.fold (fun state letter ->
                match state |> Map.tryFind letter with
                | None -> state |> Map.add letter 1
                | Some n -> state |> Map.remove letter |> Map.add letter (n+1)
                ) Map.empty
    match letterCounts |> Map.tryFind policy.Letter with
    | Some n -> n >= policy.Min && n <= policy.Max
    | None -> 0 >= policy.Min && 0 <= policy.Max
    

let validPasswords =
    System.IO.File.ReadAllLines(fileName)
    |> Array.map (fun line ->
           match line.Split(':') with
           | [| pol; pass |] -> (parsePolicy pol), pass.Trim()
           | _ -> failwithf "Invalid line: %s" line
        )
    |> Array.filter (fun (pol, pass) -> isPasswordValid pol pass)

printfn "Result: %i" (Array.length validPasswords)