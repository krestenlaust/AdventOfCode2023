module Assignment2

let firstOccurence (checks : string list) (str : string) : string Option =
    let rec iterate i =
        str.[i]