let input = Input.realInput

let groups = seq {
    let mutable group = []
    for l in input do
        if l = "" then
            yield group
            group <- []
        else
            group <- group @ [l]
    yield group
}

let distinctGroupAnswers (group: string list): char list =
    group
    |> List.collect (fun (s: string) -> s.ToCharArray() |> List.ofArray)
    |> List.distinct

let commonGroupAnswers (group: string list): char list =
    group
    |> List.collect (fun (s: string) -> s.ToCharArray() |> List.ofArray)
    |> List.groupBy id
    |> List.filter (fun (_, g) -> (List.length g) = group.Length)
    |> List.map fst

let distinctAnswers =
    groups
    |> Seq.map distinctGroupAnswers
    |> Seq.sumBy (fun g -> List.length g)
printfn $"Distinct answers: %d{distinctAnswers}"

let commonAnswers =
    groups
    |> Seq.map commonGroupAnswers
    |> Seq.sumBy (fun g -> List.length g)
printfn $"Common answers: %d{commonAnswers}"