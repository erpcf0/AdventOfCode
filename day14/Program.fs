open System.IO
open System.Text.RegularExpressions

let readLines filename =
    File.ReadAllLines filename

// type MaskBit =
//     | IgnoreBit
//     | Override1
//     | Override0

// type Mask = {
//     Value: uint64
//     MetaMask: uint64
// }
// with
//     static member empty = { Value = 0uL; MetaMask = 0uL }

// type Instruction =
//     | MaskAssignment of Mask
//     | MemoryAssignment of int * uint64

// let parseMaskBit = function
//     | 'X' -> IgnoreBit
//     | '1' -> Override1
//     | '0' -> Override0
//     | c -> failwithf "Invalid mask bit '%c'" c

// let parseMask (str: string) =
//     str.ToCharArray()
//     |> Array.map parseMaskBit
//     |> Array.rev
//     |> Array.truncate 36
//     |> Array.indexed
//     |> Array.fold (fun mask (n, maskBit) ->
//         match maskBit with
//         | Override1 ->
//             {
//                 Value = mask.Value ||| (1uL <<< n)
//                 MetaMask = mask.MetaMask ||| (1uL <<< n)
//             }
//         | Override0 ->
//             {
//                 Value = mask.Value &&& (~~~(1uL <<< n))
//                 MetaMask = mask.MetaMask ||| (1uL <<< n)
//             }
//         | IgnoreBit -> mask
//     ) Mask.empty

// let lineRegex = Regex(@"mask = ([X01]{36})|mem\[(\d+)\] = (\d+)", RegexOptions.Compiled)
// let parseLine line =
//     let m = lineRegex.Match line
//     if not m.Success then
//         failwithf "Unable to parse line '%s'" line
//     elif m.Groups.[1].Success then
//         m.Groups.[1].Value
//         |> parseMask
//         |> MaskAssignment
//     else
//         MemoryAssignment (int m.Groups.[2].Value, uint64 m.Groups.[3].Value)

// type State = {
//     Mask: Mask
//     Memory: Map<int, uint64>
// }
// with
//     static member empty = { Mask = Mask.empty; Memory = Map.empty }

// let applyMask mask n =
//     n
//     |> (|||) (mask.MetaMask &&& mask.Value)
//     |> (&&&) ((~~~mask.MetaMask) ||| mask.Value)

// let instructionFolder state instruction =
//     match instruction with
//     | MaskAssignment m -> { state with Mask = m }
//     | MemoryAssignment (loc, value) -> { state with Memory = Map.add loc (applyMask state.Mask value) state.Memory }

// /// For testing purposes.
// let uint64ToBinary i =
//     let rec appendNextBit i (sb: System.Text.StringBuilder) =
//         match i with
//         | 0uL | 1uL -> sb.Append i
//         | _ -> (appendNextBit (i / 2uL) sb).Append (i % 2uL)
//     System.Text.StringBuilder()
//     |> appendNextBit i
//     |> string

// part 2

let input = readLines "./input.txt"

type Instruction =
    | Mem of int64 * int64
    | Mask of string

let maskRegex = Regex("mask = (.*)")
let memRegex = Regex("mem\[(\d*)\] = (\d*)")
let (|AMask|_|) line =
    let m = maskRegex.Match(line)
    if m.Success
    then m.Groups.[1].Value |> Mask |> Some
    else None
let (|AMem|_|) line =
    let m = memRegex.Match(line)
    if m.Success
    then (int64 m.Groups.[1].Value, int64 m.Groups.[2].Value) |> Mem |> Some
    else None

let parse text =
    let parseLine (line : string) =
        match line with
        | AMask m -> m
        | AMem m -> m
        | _ -> failwith "Don't know how to parse line"
    text
    |> Seq.map parseLine
    |> Seq.toList

type Computer = { CurrentMask : string; Memory : Map<int64,int64> }
let init = { CurrentMask = "NO MASK"; Memory = Map.empty }
let instructions = parse input

let toBits (value : int64) =
    System.Convert.ToString(value,2).PadLeft(36,'0')

let fromBits (bits : char seq) =
    let s = bits |> Seq.map string |> String.concat ""
    System.Convert.ToInt64(s, 2)

let rec applyFloats bitstring =
    match bitstring with
    | [] -> [[]]
    | b :: bs ->
        match b with
        | '1'
        | '0' ->
            let rest = applyFloats bs
            rest |> List.map (fun a -> b :: a)
        | 'X' ->
            let rest = applyFloats bs
            let ones = rest |> List.map (fun a -> '1' :: a)
            let zeroes = rest |> List.map (fun a -> '0' :: a)
            List.append ones zeroes
        | _ -> failwith "Unknown bit"

let applyMask (mask : string) (value : int64) =
    let valuebits = toBits value

    let masked =
        Seq.zip mask valuebits
        |> Seq.map (fun (mb, vb) ->
            match mb with
            | '0' -> vb
            | '1' -> '1'
            | 'X' -> 'X'
            | _   -> ' ')

    applyFloats (masked |> List.ofSeq) |> List.map fromBits

let store computer address value =
    { computer with Memory = computer.Memory |> Map.add address value }

let runInstruction computer instruction =
    match instruction with
    | Mask m -> { computer with CurrentMask = m }
    | Mem (address, value) ->
        let destinations = applyMask computer.CurrentMask address
        destinations |> Seq.fold (fun computer address -> store computer address value) computer

let final = instructions |> Seq.fold runInstruction init

[<EntryPoint>]
let main argv =
    // let filename = Array.tryItem 0 argv |> Option.defaultValue "./input.txt"
    // let lines = readLines filename
    // printfn "Read %i lines from %s" lines.Length filename
    // let instructions = lines |> Array.Parallel.map parseLine

    // printfn ""
    // printfn "Part 1:"
    // let finalState = instructions |> Array.fold instructionFolder State.empty
    // let memSum = finalState.Memory |> Map.toSeq |> Seq.sumBy snd
    // printfn " Sum of all Memory: %i" memSum
    printfn "%A" (final.Memory |> Map.toList |> List.sumBy snd)
    0