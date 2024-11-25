namespace Bagg

module SeparateChainBag =
    open System

    type Chain<'T> = { Hash: int; Elements: 'T list }
    type Bag<'T> = { Chains: Chain<'T> list }

    let empty<'T> : Bag<'T> = { Chains = [] }

    let private hash x = abs (x.GetHashCode())

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

    let compareBags (bag1: Bag<'T>) (bag2: Bag<'T>) : bool =
        let compareChains (chain1: Chain<'T>) (chain2: Chain<'T>) =
            chain1.Hash = chain2.Hash
            && List.sort chain1.Elements = List.sort chain2.Elements

        if List.length bag1.Chains = List.length bag2.Chains then
            let sortedChains1 = bag1.Chains |> List.sortBy (fun chain -> chain.Hash)
            let sortedChains2 = bag2.Chains |> List.sortBy (fun chain -> chain.Hash)

            sortedChains1 |> List.forall2 compareChains sortedChains2
        else
            false

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

        if List.isEmpty filteredBag.Chains then
            { Chains = [] }
        else
            filteredBag

    let foldLeft (folder: 'State -> 'T -> 'State) (state: 'State) (bag: Bag<'T>) : 'State =
        bag.Chains
        |> List.fold (fun acc chain -> chain.Elements |> List.fold folder acc) state

    let foldRight folder (bag: Bag<'T>) initial =
        bag.Chains
        |> List.foldBack (fun chain acc -> chain.Elements |> List.foldBack folder acc) initial
