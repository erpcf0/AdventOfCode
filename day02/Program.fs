let input = [|
  {| Min = 1; Max = 3; Letter = 'a'; Code = "abcde" |}
  {| Min = 1; Max = 3; Letter = 'b'; Code = "cdefg" |}
  {| Min = 2; Max = 9; Letter = 'c'; Code = "ccccccccc" |}
|]

let verify (args: {|Min: int; Max: int; Letter: char; Code: string |}) =
    let count car data =
        data
        |> Seq.filter (fun x -> car = x)
        |> Seq.length
    let value = count args.Letter args.Code
    (value >= args.Min && value <= args.Max)

let result1 =
    Input.input
    |> Array.filter verify
    |> Array.length

let verify2 (args: {|Min: int; Max: int; Letter: char; Code: string |}) =
    let data = args.Code |> Seq.toList
    let min = data |> List.item (args.Min - 1)
    let max = data |> List.item (args.Max - 1)
    (args.Letter = min, args.Letter = max)

let xor = function
    | (false, true) -> true
    | (true, false) -> true
    | _ -> false

let result2 =
    Input.input
    |> Array.filter (xor << verify2)
    |> Array.length

[<EntryPoint>]
let main argv =
    printfn "%A" result2
    0
