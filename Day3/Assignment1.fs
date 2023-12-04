open System
open System.IO

let sumOfPartNumbers (engineSchematic : char list seq) =
    

let sumOfPartNumbersString (engineSchematic : string) =
    Seq.map Seq.toList (engineSchematic.Split '\n')


File.ReadAllText "input.txt"
|> sumOfPartNumbersString
|> sumOfPartNumbers