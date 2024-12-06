namespace Bagg


module SeparateChainBag =
    open System
    open System.Collections.Generic

    type Chain<'T> = { Hash: int; Elements: 'T list }
    type Bag<'T> = { Chains: Chain<'T> list }

    let empty<'T> : Bag<'T> = { Chains = [] }

    let private hash x = abs (x.GetHashCode())
    let resizeThreshold = 10 
    let resizeFactor = 2.0 
    let resizeBag (bag: Bag<'T>) : Bag<'T> =
        let totalElements = List.sumBy (fun chain -> List.length chain.Elements) bag.Chains
        let loadFactor = float totalElements / float (List.length bag.Chains)

        if loadFactor > 0.7 then
            let newSize = int (float (List.length bag.Chains) * resizeFactor)
            let newChains = Array.init newSize (fun _ -> [])

            let rec rehashChains chains =
                match chains with
                | [] -> ()
                | { Hash = h; Elements = elems } :: rest ->
                    let newHash = (h % newSize + newSize) % newSize
                    newChains.[newHash] <- newChains.[newHash] @ elems
                    rehashChains rest

            rehashChains bag.Chains
            { Chains = newChains |> Array.toList |> List.mapi (fun i elements -> { Hash = i; Elements = elements }) }
        else
            bag



    let find (item: 'T) (bag: Bag<'T>) : 'T option =
        let h = hash item

        bag.Chains
        |> List.tryFind (fun chain -> chain.Hash = h)
        |> Option.bind (fun chain -> chain.Elements |> List.tryFind (fun x -> x = item))

    let remove (item: 'T) (bag: Bag<'T>) : Bag<'T> =
        let h = hash item

        let updatedChains =
            bag.Chains
            |> List.collect (fun chain ->
                if chain.Hash = h then
                    let rec removeOne lst =
                        match lst with
                        | [] -> []
                        | x :: xs -> if x = item then xs else x :: removeOne xs

                    match removeOne chain.Elements with
                    | [] -> []
                    | updatedElements ->
                        [ { chain with
                              Elements = updatedElements } ]
                else
                    [ chain ])

        { Chains = updatedChains }

    let removeAll (item: 'T) (bag: Bag<'T>) : Bag<'T> =
        let h = hash item

        let updatedChains =
            bag.Chains
            |> List.collect (fun chain ->
                if chain.Hash = h then
                    let filteredElements = chain.Elements |> List.filter ((<>) item)

                    if filteredElements.IsEmpty then
                        []
                    else
                        [ { chain with
                              Elements = filteredElements } ]
                else
                    [ chain ])

        { Chains = updatedChains }

    let add (item: 'T) (bag: Bag<'T>) : Bag<'T> =
        let h = hash item

        let updatedChains =
            match List.tryFind (fun chain -> chain.Hash = h) bag.Chains with
            | Some chain ->
                let updatedChain =
                    { chain with
                        Elements = item :: chain.Elements }
                updatedChain :: (List.filter (fun c -> c.Hash <> h) bag.Chains)
            | None -> { Hash = h; Elements = [ item ] } :: bag.Chains

        if List.length updatedChains > resizeThreshold then
            resizeBag { Chains = updatedChains }
        else
            { Chains = updatedChains }


    let filter (predicate: 'T -> bool) (bag: Bag<'T>) : Bag<'T> =
        let updatedChains =
            bag.Chains
            |> List.choose (fun chain ->
                let filtered = List.filter predicate chain.Elements

                if filtered.IsEmpty then
                    None
                else
                    Some { chain with Elements = filtered })

        { Chains = updatedChains }

    let map (f: 'T -> 'U) (bag: Bag<'T>) : Bag<'U> =
        let updatedChains =
            bag.Chains
            |> List.map (fun chain ->
                let updatedElements = List.map f chain.Elements

                match updatedElements with
                | [] -> { Hash = 0; Elements = updatedElements }
                | head :: _ ->
                    { Hash = hash head
                      Elements = updatedElements })

        { Chains = updatedChains }
    
    let countElement (bag: Bag<'T>) (item: 'T) : int =
        bag.Chains
        |> List.sumBy (fun chain -> chain.Elements |> List.filter ((=) item) |> List.length)

    let toListWithCounter (bag: Bag<'T>) : ('T * int) list =
        bag.Chains
        |> List.collect (fun chain -> chain.Elements)
        |> List.groupBy id
        |> List.map (fun (item, occurrences) -> (item, List.length occurrences))

    let compareBags (bag1: Bag<'T>) (bag2: Bag<'T>) : bool =
        let bag1Counts = toListWithCounter bag1 |> dict
        let bag2Counts = toListWithCounter bag2 |> dict

        let allKeys = Seq.append bag1Counts.Keys bag2Counts.Keys |> Set.ofSeq

        allKeys
        |> Seq.fold (fun acc key ->
            match bag1Counts.TryGetValue(key), bag2Counts.TryGetValue(key) with
            | (true, count1), (true, count2) when count1 = count2 -> acc  
            | (true, count1), (false, _) -> acc && count1 = 0  
            | (false, _), (true, count2) -> acc && count2 = 0  
            | _ -> false) true


    let merge (bag1: Bag<'T>) (bag2: Bag<'T>) : Bag<'T> =
        let addItemToBag bag item =
            let h = hash item

            match List.tryFind (fun chain -> chain.Hash = h) bag.Chains with
            | Some chain ->
                let updatedChain =
                    { chain with
                        Elements = item :: chain.Elements }

                updatedChain :: List.filter (fun c -> c.Hash <> h) bag.Chains
            | None -> { Hash = h; Elements = [ item ] } :: bag.Chains

        let mergedBag =
            bag2.Chains
            |> List.fold
                (fun acc chain ->
                    chain.Elements
                    |> List.fold (fun b e -> { b with Chains = addItemToBag b e }) acc)
                bag1


        let filteredBag =
            { mergedBag with
                Chains = List.filter (fun chain -> not (List.isEmpty chain.Elements)) mergedBag.Chains }

 
        if List.length filteredBag.Chains > resizeThreshold then
            resizeBag filteredBag
        else
            filteredBag


    let foldLeft (folder: 'State -> 'T -> 'State) (state: 'State) (bag: Bag<'T>) : 'State =
        bag.Chains
        |> List.fold (fun acc chain -> chain.Elements |> List.fold folder acc) state

    let foldRight folder (bag: Bag<'T>) initial =
        bag.Chains
        |> List.foldBack (fun chain acc -> chain.Elements |> List.foldBack folder acc) initial
