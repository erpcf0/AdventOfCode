open System.IO

let from (input: string) =
    input.Split ","
    |> Array.filter (fun x -> "x" <> x)
    |> Array.toList
    |> List.map int

let rec transform data value acc =
    match data with
    | []      -> acc
    | x :: xs ->
        let list = Seq.initInfinite (fun y -> y * x)
        transform xs value ([(Seq.find (fun y -> y > value) list, x)] @ acc)

let result1 value data =
    let min = data |> List.map (fun (x, _) -> x) |> List.min
    let (res, ind) = data |> List.filter (fun (x, _) -> x = min) |> List.head
    (res - value) * ind

let step (input: string) =
    input.Split ","
    |> Array.indexed
    |> Array.filter (fun (_, x) -> "x" <> x)
    |> Array.toList
    |> List.map (fun (i, x) -> (i, int x))

let rec verify v acc = function
    | []           -> acc
    | (i, x) :: xs -> verify v (((i + v) = x) :: acc) xs

let rec zip acc l2 l1 =
    match l1, l2 with
    | [], [] -> acc
    | (i, x) :: xs, y :: ys -> zip (acc @ [(i, x + y)]) ys xs
    | _ -> acc

let rec result2 cons data =
    let (_, value) = List.head data
    let l = List.tail data
    if List.fold (&&) true (verify value [] l) then
        value
    else
        printfn "%A  %A" value data
        result2 cons (data |> zip [] cons)



// open Swensen.Unquote

let input = File.ReadAllLines "./input.txt"

let remainder idx busid =  (busid - (int64 idx % busid)) % busid

let parse (text : string) =
    let busids =
        text
        |> fun s -> s.Split([|','|])
        |> Seq.mapi (fun idx bus -> if bus = "x" then None else Some (idx, int64 bus))
        |> Seq.choose id
        |> Seq.map (fun (idx, busid) -> (remainder idx busid, busid))
    busids

//Chinese remainder theorem, fast version (tm)
////https://www.youtube.com/watch?v=zIFehsBHB8o
let inverse Ni divisor =
    [1L..divisor]
    |> Seq.find (fun d -> (Ni * d) % divisor = 1L)

let solve input =
    let buses = parse input
    let N = buses |> Seq.map snd |> Seq.reduce (*)
    buses
        |> Seq.map (fun (bi, divi) ->
            let Ni = N / divi
            let xi = inverse Ni divi
            let biNixi = bi * Ni * xi
            biNixi)
        |> Seq.sum
        |> (fun sum -> sum % N)


[<EntryPoint>]
let main _ =
    let data = File.ReadAllLines "./test.txt"
    // let input = data.[1] |> from
    // printfn "%A" (transform (data.[1] |> from) (int data.[0]) [] |> result1 (int data.[0]))
    // printfn "%A" (data.[1] |> step |> result2 input)
    let FINALLY = solve (input |> Seq.item 1)
    printfn "%A" FINALLY
    0 // return an integer exit code