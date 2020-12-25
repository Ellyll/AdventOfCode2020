open System
open System.Text.RegularExpressions

//let fileName = "Problem-06/problem06.test.data"
let fileName = "Problem-06/problem06.data"

let regValidLine = Regex(@"^[a-z]*$")

let readAnswers fileName =
    seq {
        let mutable group : Set<char> = Set.empty
        for line in System.IO.File.ReadLines(fileName) do
            if String.length line = 0 then
                yield group
                group <- Set.empty
            else
                if not <| regValidLine.IsMatch(line) then
                    failwithf "Invalid line: \"%s\"" line
                group <- group |> Set.union (Set.ofArray (line.ToCharArray()))
        if not (group |> Set.isEmpty) then
            yield group
    }


let result =
    readAnswers fileName
    |> Seq.sumBy (Set.count)

printfn "Result: %i" result
