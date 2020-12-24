open System
open System.Text.RegularExpressions

//let fileName = "Problem-04/problem04.test.data"
let fileName = "Problem-04/problem04.data"

let regValidLine = Regex(@"^(\S+:\S+)(\s+\S+:\S+)*$")

let readPassports fileName =
    seq {
        let mutable passport : Map<string,string> = Map.empty
        for line in System.IO.File.ReadLines(fileName) do
            if String.length line = 0 then
                yield passport
                passport <- Map.empty
            else
                if not <| regValidLine.IsMatch(line) then
                    failwithf "Invalid line: \"%s\"" line
                let pairs = line.Split(' ')
                passport <- pairs |> Array.fold (fun pass pair ->
                    let parts = pair.Split(':')
                    if Array.length parts <> 2 then
                        failwith "Parts not 2"
                    pass |> Map.add parts.[0] parts.[1]
                ) passport
        if not (passport |> Map.isEmpty) then
            yield passport
    }

let isValidPassport passport =
    let validKeys =
        [
            "byr"
            "iyr"
            "eyr"
            "hgt"
            "hcl"
            "ecl"
            "pid"
        ]
    validKeys |> List.forall (fun key -> passport |> Map.containsKey key)


let result =
    readPassports fileName
    |> Seq.filter isValidPassport
    |> Seq.length

printfn "Result: %A" result