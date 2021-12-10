let input = Input.realInput

type LateralMovement =
    | Forward
    | Backward

type SideMovement =
    | Left
    | Right

type BoardingInstruction = {
    Lateral: LateralMovement list
    Side: SideMovement list
}

let parseInstruction (i: string) =
    let parseLateralInstruction c =
        match c with
        | 'F' -> Forward
        | 'B' -> Backward
        | _ -> failwithf $"Unrecognized lateral instruction '%c{c}'"

    let parseSideInstruction c =
        match c with
        | 'L' -> Left
        | 'R' -> Right
        | _ -> failwithf $"Unrecognized side instruction '%c{c}'"

    let lateralInstructions =
        i[0..6].ToCharArray()
        |> Array.map parseLateralInstruction
        |> List.ofArray
    let sideInstructions =
        i[7..].ToCharArray()
        |> Array.map parseSideInstruction
        |> List.ofArray

    { Lateral = lateralInstructions
      Side = sideInstructions }

let rec getLateral current digit instructions =
    match instructions with
    | [ Forward ] -> current
    | [ Backward ] -> current + digit
    | Forward :: tail -> getLateral current (digit / 2) tail
    | Backward :: tail -> getLateral (current + digit) (digit / 2) tail
    | [] -> failwith "Empty list; should never happen."

let rec getSide current digit instructions =
    match instructions with
    | [ Left ] -> current
    | [ Right ] -> current + digit
    | Left :: tail -> getSide current (digit / 2) tail
    | Right :: tail -> getSide (current + digit) (digit / 2) tail
    | [] -> failwith "Empty list; should never happen."

let rowMultiplier = 8
let seatId instruction =
    let row = getLateral 0 64 instruction.Lateral
    let seat = getSide 0 4 instruction.Side

    row * 8 + seat

let seatIds =
    input
    |> Array.map parseInstruction
    |> Array.map seatId
let highestSeatId = seatIds |> Array.max
printfn $"Highest seat ID: %d{highestSeatId}"

let sortedSeatIds =
    seatIds |> Array.sort
let skipLast n a =
    Array.take (Array.length a - n) a

let gaps =
    sortedSeatIds
    |> Array.skip 1
    |> Array.zip (skipLast 1 sortedSeatIds)
    |> Array.map (fun (a, b) -> (b - a, a))
    |> Array.filter (fun (diff, _) -> diff > 1)

if gaps.Length > 1 then
    failwith "More than one gap found!"

let _, seatBeforeMine = gaps.[0]
printfn $"My seat is: %d{seatBeforeMine + 1}"
