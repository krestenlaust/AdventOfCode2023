// For more information see https://aka.ms/fsharp-console-apps
printfn "Hello from F#"

// Input:
// - Steps (List of characters, either 'R' or 'L')
// - Adjacency list
// Output:
// - Number representing the amount of steps from the Steps input, required to reach the goal.

type Entry = {Left: string; Right: string}

let mapNavigator (steps : char list) (graph : Map<string, Entry>) : int =
    let rec recursive i (currentEntry : string) =
        if currentEntry = "ZZZ" then
            i
        else
            if steps.[i] = 'R' then
                recursive (i + 1) (Map.find currentEntry graph).Right
            else
                recursive (i + 1) (Map.find currentEntry graph).Left

    recursive 0

let graphGenerator (rawGraph : string) : Map<string, Entry> =
    Map.ofList [("AAA", {Left = "BBB"; Right = "BBB"});
        ("BBB", {Left = "AAA"; Right = "ZZZ"});
        ("ZZZ", {Left = "ZZZ"; Right = "ZZZ"})]

graphGenerator ""
|> mapNavigator (Seq.toList ("LLR"))
|> printfn "%i"