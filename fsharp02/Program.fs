open FParsec
open System.IO

//let input = [| "1-3 a: abcde"
//               "1-3 b: cdefg"
//               "2-9 c: ccccccccc" |]
let input = File.ReadAllLines "input.txt"

type Policy = {
    MinimumOccurrences: int8
    MaximumOccurrences: int8
    Letter: char
}

type NewPolicy = {
    Positions: int8 list
    Letter: char
}

type Entry = Policy * string

let parseEntry entry =
    let parser: Parser<int8 * int8 * char * string, unit> =
        tuple4 pint8 (pstring "-" >>. pint8) (spaces1 >>. anyChar .>> pchar ':') (spaces1 >>. (restOfLine true))

    match run parser entry with
    | Success((min, max, char, str), _, _) -> ({ MinimumOccurrences = min; MaximumOccurrences = max; Letter = char }, str)
    | Failure(error, _, _) -> failwith error

let parsedEntries = input
                    |> Array.map parseEntry

let isCompliant ((policy, password): Entry): bool =
    let matchCount = password.ToCharArray ()
                     |> Seq.filter (fun l -> l = policy.Letter)
                     |> Seq.length
                     |> int8

    matchCount >= policy.MinimumOccurrences && matchCount <= policy.MaximumOccurrences

let compliantEntries =
    parsedEntries
    |> Seq.filter isCompliant
let compliantEntryCount = Seq.length compliantEntries

printfn $"Compliant entry count: %d{compliantEntryCount}"

let convertPolicy policy =
    { Positions = [ policy.MinimumOccurrences; policy.MaximumOccurrences ]; Letter = policy.Letter }

let isCompliantNew ((policy, password): Entry) =
    let convertedPolicy = convertPolicy policy

    let charsAtPositions = convertedPolicy.Positions
                           |> List.map (fun p -> password.[(p - 1y) |> int])
    let matchingCharacters = charsAtPositions
                             |> List.filter (fun c -> c = convertedPolicy.Letter)
    matchingCharacters.Length = 1

let compliantEntriesNew =
    parsedEntries
    |> Seq.filter isCompliantNew
let newCompliantEntryCount = Seq.length compliantEntriesNew

printfn $"New compliant entry count: %d{newCompliantEntryCount}"
