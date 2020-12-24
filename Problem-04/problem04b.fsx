open System
open System.Text.RegularExpressions

//let fileName = "Problem-04/problem04.test2.data"
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

(*
    byr (Birth Year) - four digits; at least 1920 and at most 2002.
    iyr (Issue Year) - four digits; at least 2010 and at most 2020.
    eyr (Expiration Year) - four digits; at least 2020 and at most 2030.
    hgt (Height) - a number followed by either cm or in:
        If cm, the number must be at least 150 and at most 193.
        If in, the number must be at least 59 and at most 76.
    hcl (Hair Color) - a # followed by exactly six characters 0-9 or a-f.
    ecl (Eye Color) - exactly one of: amb blu brn gry grn hzl oth.
    pid (Passport ID) - a nine-digit number, including leading zeroes.
    cid (Country ID) - ignored, missing or not.
*)
let between n1 n2 n = n >= n1 && n <= n2
let regOneOrMoreDigits = Regex(@"^\d+")
let regValidHgt = Regex(@"^(\d+)(in|cm)$")
let regValidHcl = Regex(@"^#[0-9a-f]{6}$")
let regValidEcl = Regex(@"^amb|blu|brn|gry|grn|hzl|oth$")
let regValidPid = Regex(@"^\d{9}$")
let isValidByr byr =
    regOneOrMoreDigits.IsMatch(byr) && Int32.Parse(byr) |> between 1920 2002
let isValidIyr iyr =
    regOneOrMoreDigits.IsMatch(iyr) && Int32.Parse(iyr) |> between 2010 2020
let isValidEyr eyr =
    regOneOrMoreDigits.IsMatch(eyr) && Int32.Parse(eyr) |> between 2020 2030
let isValidHgt hgt =
    regValidHgt.IsMatch(hgt) &&
        (
            (regValidHgt.Replace(hgt, "$2") = "cm" && Int32.Parse(regValidHgt.Replace(hgt, "$1")) |> between 150 193)
            || (regValidHgt.Replace(hgt, "$2") = "in" && Int32.Parse(regValidHgt.Replace(hgt, "$1")) |> between 59 76)
        )
let isValidHcl hcl = regValidHcl.IsMatch(hcl)
let isValidEcl ecl = regValidEcl.IsMatch(ecl)
let isValidPid pid = regValidPid.IsMatch(pid)

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
    let areKeysValid = validKeys |> List.forall (fun key -> passport |> Map.containsKey key)
    areKeysValid
        && isValidByr(passport |> Map.find "byr")
        && isValidIyr(passport |> Map.find "iyr")
        && isValidEyr(passport |> Map.find "eyr")
        && isValidHgt(passport |> Map.find "hgt")
        && isValidHcl(passport |> Map.find "hcl")
        && isValidEcl(passport |> Map.find "ecl")
        && isValidPid(passport |> Map.find "pid")

// let passports = readPassports fileName
// passports
// |> Seq.filter (isValidPassport)
// |> Seq.iter (fun passport ->
//         let keys = String.Join(",", passport |> Map.toList |> List.map fst |> List.sort)
//         let valid = if isValidPassport passport then "Y" else "N"
//         let pstr = passport |> Map.fold (fun str k v -> str + sprintf " %s:%s" k v) ""
//         printfn "%s: %s\t%s" valid keys pstr
//     )

let result =
    readPassports fileName
    |> Seq.filter isValidPassport
    |> Seq.length

printfn "Result: %A" result