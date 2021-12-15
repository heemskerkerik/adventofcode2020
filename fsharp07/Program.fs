open FParsec

let input = Input.realInput

type Parser<'t> = Parser<'t, unit>

let bagOrBags =
    (pstring " bag" .>> optional (pchar 's'))
let bag =
    many1CharsTill anyChar bagOrBags
let noOtherBags =
    pstring "no other bags" >>% []
let bagWithQuantity =
    pint32 .>>. (spaces1 >>. bag)
let bagList =
    noOtherBags <|> sepEndBy1 bagWithQuantity (pstring ", ")

let rule =
    bag .>>. (pstring " contain " >>. bagList .>> pchar '.')

let rules =
    sepEndBy1 rule newline

let parsedRules =
    (match run rules input with
     | Success(v, _, _) -> v
     | Failure(s, _, _) -> failwith s)
    |> Map

let rec canContain goal bag =
    let bags = parsedRules.[bag]

    if List.exists (snd >> ((=) goal)) bags then
        true
    else
        bags |> List.map snd |> List.exists (canContain goal)

let canContainShinyGold =
    parsedRules.Keys
    |> Seq.filter (canContain "shiny gold")
    |> List.ofSeq

printfn $"Number of bags that can contain 'shiny gold': %d{canContainShinyGold.Length}"

let rec getSumOfBags bag =
    parsedRules.[bag] |> List.sumBy (fun (n, bag) -> n + (n * getSumOfBags bag))

printfn "Shiny gold bags contain within them %d bags" (getSumOfBags "shiny gold")