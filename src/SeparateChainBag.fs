namespace Bagg


module SeparateChainBag =
    open System

    type Chain<'T> = { Hash: int; Elements: 'T list }
    type Bag<'T> = { Chains: Chain<'T> list }

    let empty<'T> : Bag<'T> = { Chains = [] }

    let private hash (x: 'T) (size: int) = abs (x.GetHashCode()) % size

    let resizeFactor = 2.0


    let resizeBag (bag: Bag<'T>) : Bag<'T> =
        let totalElements = List.sumBy (fun chain -> List.length chain.Elements) bag.Chains
        let currentSize = List.length bag.Chains
        let loadFactor = float totalElements / float currentSize

        if loadFactor > 0.7 then
            let newSize = max (int (float currentSize * resizeFactor)) 1
            let newChains = Array.init newSize (fun _ -> [])

            let rec rehashChains chains =
                match chains with
                | [] -> ()
                | { Elements = elems } :: rest -> 
                    elems |> List.iter (fun item ->
                        let h = hash item newSize
                        newChains.[h] <- item :: newChains.[h])
                    rehashChains rest

            rehashChains bag.Chains

            { Chains = newChains |> Array.toList |> List.mapi (fun i elements -> { Hash = i; Elements = elements }) }
        else
            bag

    let find (item: 'T) (bag: Bag<'T>) : 'T option =
        match bag.Chains with
        | [] -> None
        | _ ->
            let h = hash item (List.length bag.Chains)
            bag.Chains
            |> List.tryFind (fun chain -> chain.Hash = h)
            |> Option.bind (fun chain -> List.tryFind ((=) item) chain.Elements)

    let resizeThreshold = 10

    let remove (item: 'T) (bag: Bag<'T>) : Bag<'T> =
        let h = hash item (List.length bag.Chains)

        let updatedChains =
            bag.Chains
            |> List.collect (fun chain ->
                if chain.Hash = h then
                    match List.tryFindIndex ((=) item) chain.Elements with
                    | Some idx ->
                        let updatedElements =
                            chain.Elements
                            |> List.mapi (fun i x -> if i = idx then None else Some x)
                            |> List.choose id

                        if updatedElements.IsEmpty then
                            [] 
                        else
                            [ { chain with Elements = updatedElements } ]
                    | None ->
                        [ chain ] 
                else
                    [ chain ])

        { Chains = updatedChains }

    let add (item: 'T) (bag: Bag<'T>) : Bag<'T> =
        let size = max (List.length bag.Chains) 1
        let h = hash item size

        let updatedChains =
            match List.tryFind (fun chain -> chain.Hash = h) bag.Chains with
            | Some chain ->
                let updatedChain =
                    { chain with Elements = item :: chain.Elements }
                updatedChain :: (List.filter (fun c -> c.Hash <> h) bag.Chains)
            | None -> { Hash = h; Elements = [ item ] } :: bag.Chains

        let newBag = { Chains = updatedChains }
        if List.length updatedChains > resizeThreshold then resizeBag newBag else newBag

    let filter (predicate: 'T -> bool) (bag: Bag<'T>) : Bag<'T> =
        let updatedChains =
            bag.Chains
            |> List.choose (fun chain ->
                let filtered = List.filter predicate chain.Elements
                if filtered.IsEmpty then None else Some { chain with Elements = filtered })

        { Chains = updatedChains }

    let map (f: 'T -> 'U) (bag: Bag<'T>) : Bag<'U> =
        let size = List.length bag.Chains
        let updatedChains =
            bag.Chains
            |> List.map (fun chain ->
                let updatedElements = List.map f chain.Elements
                let h = hash (List.head updatedElements) size
                { Hash = h; Elements = updatedElements })

        { Chains = updatedChains }

    let countElement (bag: Bag<'T>) (item: 'T) : int =
        let h = hash item (List.length bag.Chains)
        bag.Chains
        |> List.sumBy (fun chain -> if chain.Hash = h then List.filter ((=) item) chain.Elements |> List.length else 0)

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
        let size = max (List.length bag1.Chains) 1 
        let hashWithSize item = hash item size

        let addItemToBag bag item =
            let h = hashWithSize item
            match List.tryFind (fun chain -> chain.Hash = h) bag.Chains with
            | Some chain ->
                let updatedChain = { chain with Elements = item :: chain.Elements }
                { bag with Chains = updatedChain :: List.filter (fun c -> c.Hash <> h) bag.Chains }
            | None ->
                { bag with Chains = { Hash = h; Elements = [ item ] } :: bag.Chains }

        let mergedBag =
            bag2.Chains
            |> List.fold (fun acc chain -> List.fold addItemToBag acc chain.Elements) bag1

        let filteredBag =
            { mergedBag with
                Chains =
                    mergedBag.Chains
                    |> List.filter (fun chain -> not (List.isEmpty chain.Elements)) }

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
