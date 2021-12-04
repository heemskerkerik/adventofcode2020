open System.IO

//let input =
//    [| "..##......."
//       "#...#...#.."
//       ".#....#..#."
//       "..#.#...#.#"
//       ".#...##..#."
//       "..#.##....."
//       ".#.#.#....#"
//       ".#........#"
//       "#.##...#..."
//       "#...##....#"
//       ".#..#...#.#" |]
let input = File.ReadAllLines "input.txt"

type Slope = {
    Right: int
    Down: int
}

let length = input.[0].Length
let defaultStride = { Right = 3; Down = 1 }
let path stride =
    input
    |> Array.mapi (fun i l -> (i, l))
    |> Array.filter (fun (i, _) -> i % stride.Down = 0)
    |> Array.mapi (fun i (_, l) -> l.[(i * stride.Right) % length])
let treeCount stride =
    path stride
    |> Array.filter (fun o -> o = '#')
    |> Array.length
    |> int64

printfn $"How many trees: %d{treeCount defaultStride}"

let strides = [
    { Right = 1; Down = 1 }
    { Right = 3; Down = 1 }
    { Right = 5; Down = 1 }
    { Right = 7; Down = 1 }
    { Right = 1; Down = 2 }
]

let multipleTreeCounts =
    strides
    |> List.map treeCount

printfn $"Raw counts: %A{multipleTreeCounts}"

let multipliedTreeCount =
    multipleTreeCounts
    |> List.fold (*) 1L

printfn $"How many trees multiplied: %d{multipliedTreeCount}"
