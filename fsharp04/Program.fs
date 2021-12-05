open System
open FParsec

let input = String.Join ('\n', Input.realInput)

type Field =
    | BirthYear
    | IssueYear
    | ExpirationYear
    | Height
    | HairColor
    | EyeColor
    | PassportId
    | CountryId

type Height =
    | Inch of byte
    | Cm of byte

let field =
    choice [ stringReturn "byr" BirthYear
             stringReturn "iyr" IssueYear
             stringReturn "eyr" ExpirationYear
             stringReturn "hgt" Height
             stringReturn "hcl" HairColor
             stringReturn "ecl" EyeColor
             stringReturn "pid" PassportId
             stringReturn "cid" CountryId ]
let fieldValueCharacter =
    choice [letter; digit; pchar '#']
let valueSeparator =
    pchar ':'
let fieldValue =
    many1Chars fieldValueCharacter

let pair: Parser<Field * string, unit> =
    field .>>. (valueSeparator >>. fieldValue)
let pairSeparator =
    choice [pchar ' '; newline]
let group =
    sepEndBy1 (attempt pair) pairSeparator
let parser =
    sepBy1 group newline

let requiredFields = [ BirthYear
                       IssueYear
                       ExpirationYear
                       Height
                       HairColor
                       EyeColor
                       PassportId ]
let rawPassports =
    match run parser input with
    | Success(v, _, _) -> v
    | Failure(err, _, _) -> failwith err

let containsRequiredFields values =
    requiredFields
    |> List.forall
           (fun rf ->
               values
               |> List.map fst
               |> List.contains rf
           )

let numberBetween min max value =
    let pnum =
        many1Chars digit |>> int16
    match run pnum value with
    | Success(v, _, _) -> v >= min && v <= max
    | Failure _ -> false

let isBirthYearValid = numberBetween 1920s 2002s
let isIssueYearValid = numberBetween 2010s 2020s
let isExpirationYearValid = numberBetween 2020s 2030s

let isHeightValid value =
    let inches =
        attempt (manyCharsTill digit (pstring "in") |>> (fun v -> Inch(v |> byte)))
    let cm =
        attempt (manyCharsTill digit (pstring "cm") |>> (fun v -> Cm(v |> byte)))
    let height =
        choice [ inches; cm ]

    match run height value with
    | Success(v, _, _) ->
        match v with
        | Cm(c) -> c >= 150uy && c <= 193uy
        | Inch(i) -> i >= 59uy && i <= 76uy
    | Failure _ -> false

let isHairColorValid value =
    let hexNum =
        many1Chars hex
    let color =
        pchar '#' >>. hexNum

    match run color value with
    | Success(v, _, _) -> v.Length = 6
    | Failure _ -> false

let isEyeColorValid value =
    let validColors = [ "amb"
                        "blu"
                        "brn"
                        "gry"
                        "grn"
                        "hzl"
                        "oth" ]
    validColors |> List.contains value

let isPassportIdValid value =
    let pnum =
        many1Chars digit

    match run pnum value with
    | Success(v, _, _) -> v.Length = 9
    | Failure _ -> false

let isCountryIdValid _ = true

let allFieldsValid passport =
    let isFieldValid (field, value) =
        let f =
            match field with
            | BirthYear -> isBirthYearValid
            | IssueYear -> isIssueYearValid
            | ExpirationYear -> isExpirationYearValid
            | Height -> isHeightValid
            | HairColor -> isHairColorValid
            | EyeColor -> isEyeColorValid
            | PassportId -> isPassportIdValid
            | CountryId -> isCountryIdValid

        f value

    passport |> List.forall isFieldValid

let passportsWithRequiredFields =
    rawPassports
    |> List.filter containsRequiredFields

let numberOfPassportsWithRequiredFields = passportsWithRequiredFields |> List.length
printfn $"Number of passports with required fields: %d{numberOfPassportsWithRequiredFields}"

let validPassports =
    rawPassports
    |> List.filter containsRequiredFields
    |> List.filter allFieldsValid

let numberOfValidPassports = validPassports |> List.length
printfn $"Number of valid passports: %d{numberOfValidPassports}"
