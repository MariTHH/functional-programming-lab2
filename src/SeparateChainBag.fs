namespace Bagg

module SeparateChainBag =
    open System

    // Определение цепочки для элементов с одинаковым хэшем
    type Chain<'T> = { Hash: int; Elements: 'T list }

    // Определение Bag
    type Bag<'T> = { Chains: Chain<'T> list }

    // Создание пустого Bag
    let empty<'T> : Bag<'T> = { Chains = [] }

    // Хэш-функция
    let private hash x = abs (x.GetHashCode())

    // Сортировка цепочек и их элементов
    let private sortBag (bag: Bag<'T>) : Bag<'T> =
        let sortedChains =
            bag.Chains
            |> List.map (fun chain ->
                { chain with
                    Elements = List.sort chain.Elements }) // Сортируем элементы в цепочке
            |> List.sortBy (fun chain -> chain.Hash) // Сортируем цепочки по хэшу

        { Chains = sortedChains }

    // Поиск цепочки по хэшу
    let find (item: 'T) (bag: Bag<'T>) : 'T option =
        let h = hash item

        bag.Chains
        |> List.tryFind (fun chain -> chain.Hash = h)
        |> Option.bind (fun chain -> chain.Elements |> List.tryFind (fun x -> x = item))

    // Удаление одного экземпляра по хэшу (аналог s.erase(s.find(x)))
    let remove (item: 'T) (bag: Bag<'T>) : Bag<'T> =
        let h = hash item

        let updatedChains =
            bag.Chains
            |> List.collect (fun chain ->
                if chain.Hash = h then
                    // Удаляем только один экземпляр
                    let rec removeOne lst =
                        match lst with
                        | [] -> []
                        | x :: xs -> if x = item then xs else x :: removeOne xs

                    match removeOne chain.Elements with
                    | [] -> [] // Удаляем цепочку, если она пуста
                    | updatedElements ->
                        [ { chain with
                              Elements = updatedElements } ]
                else
                    [ chain ])

        { Chains = updatedChains } |> sortBag


    // Удаление всех экземпляров элемента (аналог s.erase(x))
    let removeAll (item: 'T) (bag: Bag<'T>) : Bag<'T> =
        let h = hash item

        let updatedChains =
            bag.Chains
            |> List.filter (fun chain -> chain.Hash <> h || not (List.exists ((=) item) chain.Elements))

        { Chains = updatedChains } |> sortBag

    // Добавление элемента
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

        { Chains = updatedChains } |> sortBag

    // Фильтрация
    let filter (predicate: 'T -> bool) (bag: Bag<'T>) : Bag<'T> =
        let updatedChains =
            bag.Chains
            |> List.choose (fun chain ->
                let filtered = List.filter predicate chain.Elements

                if filtered.IsEmpty then
                    None
                else
                    Some { chain with Elements = filtered })

        { Chains = updatedChains } |> sortBag

    // Отображение (map)
    let map (f: 'T -> 'U) (bag: Bag<'T>) : Bag<'U> =
        let updatedChains =
            bag.Chains
            |> List.map (fun chain ->
                // Безопасно обрабатываем пустую цепочку
                let updatedElements = List.map f chain.Elements

                match updatedElements with
                | [] -> { Hash = 0; Elements = updatedElements } // Можно назначить дефолтный хэш, если список пуст
                | head :: _ ->
                    { Hash = hash head
                      Elements = updatedElements })

        { Chains = updatedChains } |> sortBag


    // Свойства моноида (слияние Bag)
    let merge (bag1: Bag<'T>) (bag2: Bag<'T>) : Bag<'T> =
        // Добавление элемента в мешок
        let addItemToBag bag item =
            let h = hash item

            match List.tryFind (fun chain -> chain.Hash = h) bag.Chains with
            | Some chain ->
                let updatedChain =
                    { chain with
                        Elements = item :: chain.Elements }

                updatedChain :: List.filter (fun c -> c.Hash <> h) bag.Chains
            | None -> { Hash = h; Elements = [ item ] } :: bag.Chains

        // Добавляем элементы из второго мешка в первый
        let mergedBag =
            bag2.Chains
            |> List.fold
                (fun acc chain ->
                    chain.Elements
                    |> List.fold (fun b e -> { b with Chains = addItemToBag b e }) acc)
                bag1

        // Удаляем пустые цепочки, включая пустые цепочки, которые получаются после слияния
        let filteredBag =
            { mergedBag with
                Chains = List.filter (fun chain -> not (List.isEmpty chain.Elements)) mergedBag.Chains }

        // Если в итоге пустых цепочек нет, мешок должен быть пустым
        if List.isEmpty filteredBag.Chains then
            { Chains = [] }
        else
            // Сортируем по хэшу
            sortBag filteredBag

    // Левосторонняя свёртка (с сохранением)
    let foldLeft (folder: 'State -> 'T -> 'State) (state: 'State) (bag: Bag<'T>) : 'State =
        bag.Chains
        |> List.fold (fun acc chain -> chain.Elements |> List.fold folder acc) state

    // Правосторонняя свёртка
    let foldRight folder (bag: Bag<'T>) initial =
        bag.Chains
        |> List.foldBack (fun chain acc -> chain.Elements |> List.foldBack folder acc) initial
