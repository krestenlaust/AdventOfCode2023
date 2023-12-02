open System
open System.IO

let stringToCharArray (str : string) : char list =
    str |> Seq.toList

let charArrayToString (str : char list) : string =
    str |> Array.ofList |> String

let rec exclusivelyNumbers =
    List.filter Char.IsDigit

let firstAndLastElement str =
    [Seq.head str; Seq.last str]

let findFirstAndLastNumber str =
    str
    |> stringToCharArray
    |> exclusivelyNumbers
    |> firstAndLastElement
    |> charArrayToString

let calibrationValue =
    findFirstAndLastNumber >> int

let sumOfAllLines =
    Seq.sumBy calibrationValue

File.ReadLines "input.txt"
|> sumOfAllLines
|> printfn "%i"