open ScBag

[<EntryPoint>]
let main argv =
    let bag1 = Empty |> add "a" 1 |> add "a" 2 |> add "b" 3 |> add "c" 5
    let baggy1= Empty |> add "hello" "ma" |> add "porche" "ma" |> add "h" "d" |> add "m" "f"
    let baggy2 = Empty|> add 1 1 |> add 2 1 
    let baggy3 = Empty|> add baggy1 1 |> add baggy1 1 

    let filtered1 = filter (fun x -> x > 1) bag1
    let mapped1 = map (fun x -> x * 2) bag1
    
    printfn "string string %A" baggy1
    printfn "int int %A" baggy2
    printfn "scbag scbag %A" baggy3
    printfn "Original (bag1): %A" bag1
    printfn "Filtered (bag1): %A" filtered1
    printfn "Mapped (bag1): %A" mapped1

    let bag2 = Empty |> add "x" 10 |> add "y" 5 |> add "y" 7
    
    let filtered2 = filter (fun x -> x < 7) bag2
    
    let mapped2 = map (fun x -> x * 3) bag2
    
    printfn "Original (bag2): %A" bag2
    printfn "Filtered (bag2): %A" filtered2
    printfn "Mapped (bag2): %A" mapped2

    let bag3 = Empty |> add "m" 4 |> add "n" 8 |> add "o" 1 |> add "m" 2
    
    let removed = remove "m" 2 bag3
    
    let filtered3 = filter (fun x -> x = 4 || x = 1) removed
    
    let mapped3 = map (fun x -> x * 5) removed

    printfn "Original (bag3): %A" bag3
    printfn "Removed (from bag3): %A" removed
    printfn "Filtered (from bag3): %A" filtered3
    printfn "Mapped (from bag3): %A" mapped3

    let sumLeft = foldLeft (+) 0 bag1
    printfn "Sum (foldLeft) of bag1: %d" sumLeft 
    
    let sumRight = foldRight (-) bag1 0
    printfn "Sum (foldRight) of bag1: %d" sumRight 

    let bag2 = Empty |> add "x" 5 |> add "y" 10
    let bag3 = Empty |> add "a" 1 |> add "b" 2

    let combinedBag = mappend bag2 bag3
    printfn "Combined Bag (mappend of bag2 and bag3): %A" combinedBag

    let bag4 = Empty |> add "z" 3
    let associativityCheck1 = mappend (mappend bag2 bag3) bag4
    let associativityCheck2 = mappend bag2 (mappend bag3 bag4)
    printfn "Associativity check (left): %A" associativityCheck1
    printfn "Associativity check (right): %A" associativityCheck2

    let neutralCheck1 = mappend bag2 mempty
    let neutralCheck2 = mappend mempty bag2
    printfn "Neutral element check (bag2 with mempty): %A" neutralCheck1
    printfn "Neutral element check (mempty with bag2): %A" neutralCheck2
    
    0
