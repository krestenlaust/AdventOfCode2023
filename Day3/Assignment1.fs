open System
open System.IO

let clamp (min : int) (max : int) (value : int) =
    Math.Max(Math.Min(value, max), min)

// 534316
let rec findStringNumber = function
    | str when Char.IsDigit (str |> Seq.head) -> 
        string (str |> Seq.head) + findStringNumber (str |> Seq.skip 1)
    | _ -> ""

let rowStart i width =
    i - i % width

let rowEnd i width =
    rowStart i width + width

let containsNutsOrBolts =
    Seq.exists (fun ch -> ch <> '.' && ch <> '\n' && not (Char.IsDigit ch))

let checkRow (str : char seq) (width : int) i checkLength =
    str
    |> Seq.skip(clamp(rowStart(i, width), rowEnd(i, width), i))
    |> Seq.truncate checkLength
    |> containsNutsOrBolts
    
let checkNumberSchematic (schematic : string) width i numLength =
    checkRow schematic width (i - 1) (numLength + 2)
    || checkRow schematic width (i - 1 - width) (numLength + 2)
    || checkRow schematic width (i - 1 + width) (numLength + 2)

let iterateNumbersInSchematic (schematic : string) width =
    let rec iterateRec i acc =
        if i >= schematic.Length then
            acc
        else
            if Char.IsDigit schematic.[i] then
                let num = findStringNumber (Seq.skip i schematic)
                if checkNumberSchematic schematic width i num.Length then
                    iterateRec (i + num.Length) (acc + (num |> int))
                else
                    iterateRec (i + 1) acc
            else
                iterateRec (i + 1) acc

    iterateRec 0 0
    
let sumSchematicNumbers (schematic : string) =
    iterateNumbersInSchematic schematic (schematic.IndexOf '\n' + 1)

File.ReadAllText "input.txt"
|> sumSchematicNumbers
|> printfn "%d"
