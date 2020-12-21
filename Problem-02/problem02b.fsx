open System.Text.RegularExpressions

type Policy =
    {
        Position1: int
        Position2: int
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
        Position1 = System.Int32.Parse(regPolicy.Replace(str, "$1"))
        Position2 = System.Int32.Parse(regPolicy.Replace(str, "$2"))
        Letter = regPolicy.Replace(str, "$3").ToCharArray().[0]
    }

let isPasswordValid policy (password: string) =
    if policy.Position1 > String.length password then
        false
    elif policy.Position2 > String.length password then
        false
    else
        let pos1isMatch = password.[policy.Position1 - 1] = policy.Letter
        let pos2isMatch = password.[policy.Position2 - 1] = policy.Letter
        (pos1isMatch || pos2isMatch) && pos1isMatch <> pos2isMatch
    

let validPasswords =
    System.IO.File.ReadAllLines(fileName)
    |> Array.map (fun line ->
           match line.Split(':') with
           | [| pol; pass |] -> (parsePolicy pol), pass.Trim()
           | _ -> failwithf "Invalid line: %s" line
        )
    |> Array.filter (fun (pol, pass) -> isPasswordValid pol pass)

printfn "Result: %i" (Array.length validPasswords)