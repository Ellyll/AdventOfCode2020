open System
open System.IO
open System.Text.RegularExpressions

//let fileName = "Problem-07/problem07.test.data"
let fileName = "Problem-07/problem07.data"

let ruleReg = Regex(@"^(.*) bags contain (.*)\.$")
let bagRuleReg = Regex(@"^(\d+) (.*)$")
let parseRule line =
    if not (ruleReg.IsMatch(line)) then
        failwithf "Invalid line: %s" line
    let bagName = ruleReg.Replace(line, "$1")
    let bagRules =
        ruleReg.Replace(line, "$2").Replace("no other bags", "").Replace("bags", "").Replace("bag", "").Split(",")
        |> Array.map (fun s -> s.Trim())
        |> Array.filter (fun s -> String.length s > 0)
        |> Array.map (fun s ->
            let number = bagRuleReg.Replace(s, "$1")
            let name = bagRuleReg.Replace(s, "$2")
            (name, number)
            )
        |> Map.ofArray
    (bagName, bagRules)

let rec canContain bagName target rules =
    let bagRules = rules |> Map.find bagName
    bagRules
    |>Map.exists (fun k _ ->
        if k = target then
            true
        else
            canContain k target rules
        )

let getRules fileName =
    File.ReadAllLines(fileName)
    |> Seq.map(parseRule)
    |> Map.ofSeq

let rules = getRules fileName

let result =
    rules
    |> Map.toList
    |> List.map fst
    |> List.filter (fun bagName -> canContain bagName "shiny gold" rules)
    |> List.length

printfn "Result: %i" result
