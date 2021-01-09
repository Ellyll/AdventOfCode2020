open System
open System.Text.RegularExpressions

//let fileName = "Problem-16/problem16a.test.data"
let fileName = "Problem-16/problem16.data"

type ReadState =
    | ExpectingRules
    | ExpectingYourTicket
    | ExpectingNearByTicket

type Rule =
    {
        Field: string
        Function: int -> bool
    }

let between a b n =
    n >= a && n <= b

let ruleReg = Regex(@"^([^:]+):\s*(\d+)-(\d+)\s+or\s+(\d+)-(\d+)\s*$")
// e.g.
// class: 1-3 or 5-7
let parseRule line =
    if not (ruleReg.IsMatch(line)) then
        failwithf "Unable to parse line as rule: %s" line
    let field = ruleReg.Replace(line, "$1")
    // a1-b1 or a2-b2
    let a1 = (ruleReg.Replace(line, "$2") |> Int32.Parse)
    let b1 = (ruleReg.Replace(line, "$3") |> Int32.Parse)
    let a2 = (ruleReg.Replace(line, "$4") |> Int32.Parse)
    let b2 = (ruleReg.Replace(line, "$5") |> Int32.Parse)
    let fn n = (n |> between a1 b1) || (n |> between a2 b2)
    { Field = field ; Function = fn }

let parseValues (values: string) =
    values.Split(',')
    |> Array.map (Int32.Parse)
    |> Array.toList

let getTicketScanningErrorRate rules tickets =
    tickets
    |> List.fold (fun total ticket ->
            (ticket
            |> List.filter (fun v ->
                    rules |> List.exists (fun rule -> rule.Function v )
                    |> not
                )
            |> List.sum ) + total
            ) 0


let rules, yourTicket, nearbyTickets, _ =
    IO.File.ReadAllLines(fileName)
    |> Array.fold (fun (rules, yourTicket, nearbyTickets, readState) line ->
        if line.StartsWith("your ticket:") then
            (rules, yourTicket, nearbyTickets, ExpectingYourTicket)
        elif line.StartsWith("nearby tickets:") then
            (rules, yourTicket, nearbyTickets, ExpectingNearByTicket)
        elif String.IsNullOrWhiteSpace(line) then
            (rules, yourTicket, nearbyTickets, readState)
        else
            match readState with
            | ExpectingRules -> (((parseRule line)::rules),yourTicket,nearbyTickets,readState)
            | ExpectingYourTicket -> (rules, (parseValues line), nearbyTickets, readState)
            | ExpectingNearByTicket -> (rules, yourTicket, (parseValues line)::nearbyTickets, readState)
            ) ([],[],[],ExpectingRules)

let result = getTicketScanningErrorRate rules nearbyTickets
printfn "Result: %i" result
