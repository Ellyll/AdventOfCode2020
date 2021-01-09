open System
open System.Text.RegularExpressions

//let fileName = "Problem-16/problem16b.test.data"
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

let validTickets =
    nearbyTickets
    |> List.filter (fun ticket ->
            ticket |> List.forall (fun v -> rules |> List.exists (fun rule -> rule.Function v))
        )

let fieldMap =
    let possibilities =
        rules
        |> List.map (fun rule ->
                let validColumns =
                    [ 0..((List.length yourTicket)-1) ]
                    |> List.filter (fun col ->
                        yourTicket::validTickets
                        |> List.forall (fun vt -> rule.Function vt.[col]))
                    |> Set.ofList
                (rule.Field,validColumns)           
            )
        |> Map.ofList
    let rec loop mappedFields upmappedFields =
        if Map.isEmpty upmappedFields then
            mappedFields
        else
            let field, poss =
                upmappedFields
                |> Map.toList
                |> List.minBy (fun (_,s) -> s |> Set.count)
            if Set.isEmpty poss then
                failwithf "Oops nothing found for field %s" field
            let column = poss |> Set.minElement
            let mappedFields' = mappedFields |> Map.add field column
            let upmappedFields' =
                upmappedFields
                |> Map.remove field
                |> Map.map (fun _ ps -> ps |> Set.remove column)
            loop mappedFields' upmappedFields'
    loop Map.empty possibilities


let ticketMap =
    fieldMap
    |> Map.map (fun k v -> yourTicket.[v] )

let result =
    ticketMap
    |> Map.toList
    |> List.filter (fun (f,_) -> f.StartsWith("departure"))
    |> List.map (snd >> int64)
    |> List.fold (*) 1L
printfn "Result: %i" result
