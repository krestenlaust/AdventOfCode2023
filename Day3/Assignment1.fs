﻿open System
open System.IO

let rec findStringNumber = function
    | str when Char.IsDigit (str |> Seq.head) -> 
        Seq.append
            (findStringNumber (str |> Seq.skip 1))
            (Seq.singleton (str |> Seq.head))
    | _ -> Seq.empty

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
    let rec iterateRec i =
        if i >= schematic.Length then
            0
        else
            (
            if Char.IsDigit schematic.[i] then
                let num = findStringNumber schematic.[i..] |> string
                if checkNumberSchematic schematic width i num.Length then
                    (num |> int)
                else
                    0
            else
                0
            ) + iterateRec (i + 1)

    iterateRec 0

let sumSchematicNumbers (schematic : string) =
    iterateNumbersInSchematic schematic (schematic.IndexOf '\n' + 1)

File.ReadAllText "input.txt"
|> sumSchematicNumbers
|> printfn "%d"


findStringNumber "123..1"
|> string
|> printfn "%s"

(*
let to1D (x : int) (y : int) (width : int) : int =
    y * width + x

let findNotDotOrDigitAroundChar (str : string) width absoluteX absoluteY numberLength =
    let rec offsetRec offsetX offsetY =
        let x = offsetX + absoluteX
        let y = offsetY + absoluteY
        let i = to1D x y width

        if offsetX = numberLength + 1 then
            offsetRec 0 (offsetY + 1)
        elif offsetY = 2 then
            false
        elif x < 0 || y < 0 || x >= width || i >= str.Length then
            offsetRec (offsetX + 1) offsetY
        else
            let ch = str.[i]

            (ch <> '.' && ch <> '\n' && not (Char.IsDigit ch)) || offsetRec (offsetX + 1) offsetY

    offsetRec -1 -1

let rec iterate2Dstring width height x y (str : string) =
    if x = width then
        iterate2Dstring width height 0 (y + 1) str
    elif y = height then
        0
    else
        let i = to1D x y width

        if Char.IsDigit str.[i] then
            let num = findStringNumber str.[i..]
            if findNotDotOrDigitAroundChar str width x y num.Length then
                (num |> int) + iterate2Dstring width height (x + num.Length) y str
            else
                iterate2Dstring width height (x + num.Length) y str
        else
            iterate2Dstring width height (x + 1) y str

let count x = Seq.filter ((=) x) >> Seq.length

let calculateEngineSchematic (str : string) =
    iterate2Dstring (str.IndexOf '\n' + 1) (count '\n' str) 0 0 str

File.ReadAllText "input.txt"
|> calculateEngineSchematic
|> printfn "%d"
*)