open System.IO

let expenses = File.ReadAllLines "input.txt"
               |> Seq.map int

let combinations = expenses
                   |> Seq.allPairs expenses
let tripleCombinations = combinations
                         |> Seq.allPairs expenses
                         |> Seq.map (fun (a, (b, c)) -> (a, b, c))

let a, b = combinations
           |> Seq.find (fun (a, b) -> a + b = 2020)
printfn $"Product: %d{a * b}"

let a', b', c = tripleCombinations
              |> Seq.find (fun (a, b, c) -> a + b + c = 2020)
printfn $"Triple product: %d{a' * b' * c}"