open System.IO

let readLines filename =
    File.ReadAllLines filename

module List =
    let rec permutationPairs source =
        match source with
        | [] -> []
        | head :: tail -> List.collect (fun v -> [head, v]) tail @ permutationPairs tail

    let toSeqOfTails source =
        source
        |> Seq.unfold (fun remianingList ->
            match remianingList with
            | [] -> None
            | _ :: tail -> Some (remianingList, tail)
        )

let containsSumOfTwoUniqueInSpan expectedSum arr =
    List.ofArray arr
    |> List.permutationPairs
    |> List.exists (fun (a, b) -> a + b = expectedSum)

let validateIntAtIndex (ints: int64[]) index integer =
    if index < 25 then
        true
    else
        containsSumOfTwoUniqueInSpan integer ints.[index-25..index-1]

module Seq =
    let foldWhileSome folder state source =
        source
        |> Seq.scan (fun prevState item ->
            folder (Option.get prevState) item
        ) (Some state)
        |> Seq.takeWhile Option.isSome
        |> Seq.map Option.get
        |> Seq.last

let inline sumWhileBelow maxSum ints =
    ints
    |> Seq.foldWhileSome (fun (intsSet, sum) i ->
        if i + sum > maxSum then
            None
        else
            Some (Set.add i intsSet, sum + i)
    ) (Set.empty, LanguagePrimitives.GenericZero)

let findSetWithSum maxSum ints =
    ints
    |> List.ofArray
    |> List.toSeqOfTails
    |> Seq.map (sumWhileBelow maxSum)
    |> Seq.find (snd >> ((=) maxSum))

[<EntryPoint>]
let main _ =
    let lines = readLines "./input.txt"
    printfn "Read %i lines" lines.Length

    let ints = Array.map int64 lines

    printfn ""
    printfn "Part 1:"
    let validated = Array.Parallel.mapi (validateIntAtIndex ints) ints
    let firstInvalidIndex = validated |> Array.findIndex ((=) false)
    let firstInvalidInteger = ints.[firstInvalidIndex]
    printfn " First invalid at index: %i" firstInvalidIndex
    printfn " First invalid integer: %i" firstInvalidInteger

    printfn ""
    printfn "Part 2:"
    let encrWeaknessSet, sum =
        ints.[25..]
        |> findSetWithSum firstInvalidInteger
    printfn " Set (length=%i): %A" encrWeaknessSet.Count encrWeaknessSet
    printfn " Sum: %i" sum
    printfn " Min: %i" encrWeaknessSet.MinimumElement
    printfn " Max: %i" encrWeaknessSet.MaximumElement
    printfn " Sum(Min, Max): %i" (encrWeaknessSet.MinimumElement + encrWeaknessSet.MaximumElement)

    0