open System
open System.IO

let rec findStringNumber = function
    | str when Char.IsDigit (str |> Seq.head) -> 
        string (str |> Seq.head) + findStringNumber (str |> Seq.skip 1)
    | _ -> ""

let containsNutsOrBolts =
    Seq.exists (fun ch -> ch <> '.' || ch <> '\n' || not (Char.IsDigit ch))

let checkRow (str : char seq) i checkLength =
    str
    |> Seq.skip (Math.Max(0, i))
    |> Seq.truncate checkLength
    |> containsNutsOrBolts
    
let checkNumberSchematic (schematic : string) width i numLength =
    checkRow schematic (i - 1) (numLength + 2)
    || checkRow schematic (i - 1 - width) (numLength + 2)
    || checkRow schematic (i - 1 + width) (numLength + 2)

let iterateNumbersInSchematic (schematic : string) width =
    let rec iterateRec i acc =
        if i >= schematic.Length then
            acc
        else
            let newAcc =
                if Char.IsDigit schematic.[i] then
                    let num = findStringNumber (Seq.skip i schematic)
                    if checkNumberSchematic schematic width i num.Length then
                        acc + (num |> int)
                    else
                        acc
                else
                    acc
            
            iterateRec (i + 1) newAcc

    iterateRec 0 0
    
let sumSchematicNumbers (schematic : string) =
    iterateNumbersInSchematic schematic (schematic.IndexOf '\n' + 1)

File.ReadAllText "input.txt"
|> sumSchematicNumbers
|> printfn "%d"
