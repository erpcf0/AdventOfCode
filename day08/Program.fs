open System.IO

let filtered (input: string []) =
    input
    |> Array.filter (fun x -> x.Contains "jmp" || x.Contains "nop")

let rec operation (input: string []) pos lpos acc =
    let value = input.[pos].Split ' '
    if List.contains pos lpos then
        acc
    else
        match value.[0], value.[1] with
        | "nop", _ -> operation input (pos + 1) (lpos @ [pos]) acc
        | "jmp", p -> operation input (pos + int p) (lpos @ [pos]) acc
        | "acc", v -> operation input (pos + 1) (lpos @ [pos]) (acc + int v)
        | _ -> acc

let rec operationDeep (input: string []) pos lpos acc eval =
    if List.contains pos lpos || pos > input.Length - 1 then
            (acc, pos)
    else
        let value = input.[pos].Split ' '
        if input.[pos] <> eval then
            match value.[0], value.[1] with
            | "nop", _ -> operationDeep input (pos + 1) (lpos @ [pos]) acc eval
            | "jmp", p -> operationDeep input (pos + int p) (lpos @ [pos]) acc eval
            | "acc", v -> operationDeep input (pos + 1) (lpos @ [pos]) (acc + int v) eval
            | _ -> (acc, pos)
        else
            match value.[0], value.[1] with
            | "jmp", _ -> operationDeep input (pos + 1) (lpos @ [pos]) acc eval
            | "nop", p -> operationDeep input (pos + int p) (lpos @ [pos]) acc eval
            | _ -> (acc, pos)

let result2 (input: string []) =
    input
    |> filtered
    |> Array.map (fun x -> operationDeep input 0 [] 0 x)
    |> Array.filter (fun (_, x) -> x = input.Length)

[<EntryPoint>]
let main argv =
    // let data = File.ReadAllLines "./test.txt"
    let data = File.ReadAllLines "./input.txt"
    // printfn "%A" (operation data 0 [] 0)
    printfn "%A" (result2 data)
    0 // return an integer exit code
