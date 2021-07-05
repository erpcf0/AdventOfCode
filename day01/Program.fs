let input1 = [1721;979;366;299;675;1456]

let rec result1 x = function
    | [] -> None
    | y :: _ as data when (List.contains (x - y) data) ->
        Some (y * (x - y))
    | _ :: ys ->
        result1 x ys

let rec result2 data =
    let temp = result1 (2020 - List.head data) (List.tail data)
    match temp with
    | Some x -> (List.head data) * x
    | None   -> result2 (List.tail data)

[<EntryPoint>]
let main argv =
    // printfn "%A" (result1 2020 Input.input)
    printfn "%A" (result2 input1)
    0 // return an integer exit code
