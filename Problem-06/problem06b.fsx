open System
open System.Text.RegularExpressions

//let fileName = "Problem-06/problem06.test.data"
let fileName = "Problem-06/problem06.data"

let regValidLine = Regex(@"^[a-z]*$")

let readAnswers fileName : seq<seq<Set<char>>> =
    seq {
        let mutable group = ResizeArray<Set<char>>()
        for line in System.IO.File.ReadLines(fileName) do
            if String.length line = 0 then
                yield group
                group <- ResizeArray<Set<char>>()
            else
                if not <| regValidLine.IsMatch(line) then
                    failwithf "Invalid line: \"%s\"" line
                group.Add(Set.ofArray (line.ToCharArray()))
               
        if group.Count <> 0 then
            yield group
    }


let result =
    readAnswers fileName
    |> Seq.map (Set.intersectMany)
    |> Seq.sumBy (Set.count)

printfn "Result: %i" result
