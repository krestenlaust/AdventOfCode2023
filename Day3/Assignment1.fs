open System
open System.IO

let to1D (x : int) (y : int) (width : int) : int =
    y * width + x

let rec findNotDotOrDigitAroundChar (str : string) width absoluteX absoluteY numberLength =
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

let rec findStringNumber (numberWithString : string) =
    if numberWithString.Length = 0 then
        ""
    elif Char.IsDigit(numberWithString.[0]) then 
        string numberWithString.[0] + findStringNumber(numberWithString.[1..])
    else
        ""

findStringNumber "123..1"
|> printfn "%s"


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