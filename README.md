
## Лабораторная работа №2 по F# Толстых М.А.

## `sc-bag`

### Цель

Цель: освоиться с построением пользовательских типов данных, полиморфизмом, рекурсивными алгоритмами и средствами тестирования (unit testing, property-based testing).

В рамках лабораторной работы вам предлагается реализовать одну из предложенных классических структур данных (список, дерево, бинарное дерево, hashmap, граф...).

Требования:

- Функции:
    - добавление и удаление элементов;
    - фильтрация;
    - отображение (map);
    - свертки (левая и правая);
    - структура должна быть моноидом.
- Структуры данных должны быть неизменяемыми.
- Библиотека должна быть протестирована в рамках unit testing.
- Библиотека должна быть протестирована в рамках property-based тестирования (как минимум 3 свойства, включая свойства моноида).
- Структура должна быть полиморфной.
- Требуется использовать идиоматичный для технологии стиль программирования. Примечание: некоторые языки позволяют получить большую часть API через реализацию небольшого интерфейса. Так как лабораторная работа про ФП, а не про экосистему языка -- необходимо реализовать их вручную и по возможности -- обеспечить совместимость.

---

### Описание задач

---

## Реализация

### Добавление и удаление элементов

```fsharp
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
```

```fsharp
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
```

### Тест добавления и удаления элементов

```fsharp
    [<Test>]
    member _.``Add element to empty bag should contain the element``() =
        let bag = empty<int>
        let updatedBag = add 1 bag
        let found = find 1 updatedBag
        Assert.AreEqual(Some 1, found)

    [<Test>]
    member _.``Remove element from bag should remove it``() =
        let bag = [ 1; 2; 2; 3 ] |> List.fold (fun acc e -> add e acc) empty<int>
        let updatedBag = removeAll 2 bag
        let found = find 2 updatedBag
        Assert.AreEqual(None, found)

    [<Test>]
    member _.``Remove element from bag should not throw error and remove only one element``() =
        let bag = empty<int>
        let bag1 = bag |> add 3 |> add 3
        let updatedBag = remove 3 bag1
        Assert.AreEqual(Some 3, find 3 updatedBag)

```

### Фильтрация

```fsharp
    let filter (predicate: 'T -> bool) (bag: Bag<'T>) : Bag<'T> =
        let updatedChains =
            bag.Chains
            |> List.choose (fun chain ->
                let filtered = List.filter predicate chain.Elements
                if filtered.IsEmpty then None
                else Some { chain with Elements = filtered })
        { Chains = updatedChains } |> sortBag
```

### Тест фильтрации

```fsharp
    [<Test>]
    member _.``Filter should only include matching elements``() =
        let bag = [ 1; 2; 3; 4 ] |> List.fold (fun acc e -> add e acc) empty<int>
        let filteredBag = filter (fun x -> x % 2 = 0) bag
        Assert.AreEqual(Some 2, find 2 filteredBag)
        Assert.AreEqual(Some 4, find 4 filteredBag)
        Assert.AreEqual(None, find 1 filteredBag)
        Assert.AreEqual(None, find 3 filteredBag)
```

#### Отображение(map)

```fsharp
    let map (f: 'T -> 'U) (bag: Bag<'T>) : Bag<'U> =
        let updatedChains =
            bag.Chains
            |> List.map (fun chain ->
                let updatedElements = List.map f chain.Elements
                match updatedElements with
                | [] -> { Hash = 0; Elements = updatedElements } 
                | head :: _ -> { Hash = hash head; Elements = updatedElements })
        { Chains = updatedChains } |> sortBag
```

#### Свертки (левая и правая)

```fsharp
    let foldLeft (folder: 'State -> 'T -> 'State) (state: 'State) (bag: Bag<'T>) : 'State =
        bag.Chains
        |> List.fold (fun acc chain ->
            chain.Elements |> List.fold folder acc
        ) state

    let foldRight folder (bag: Bag<'T>) initial =
        bag.Chains
        |> List.foldBack (fun chain acc ->
            chain.Elements |> List.foldBack folder acc
        ) initial
```

### Тест свертки

```fsharp
    [<Test>]
    member _.``Fold left should accumulate all elements property test``() =
        let property (elements: int list) =
            let bag = elements |> List.fold (fun acc e -> add e acc) empty<int>
            let sum = foldLeft (+) 0 bag
            sum = List.sum elements

        Check.Quick property
```

### Структура должна быть моноидом.

```fsharp
    let empty<'T> : Bag<'T> = { Chains = [] }

    let merge (bag1: Bag<'T>) (bag2: Bag<'T>) : Bag<'T> =
        let addItemToBag bag item =
            let h = hash item
            match List.tryFind (fun chain -> chain.Hash = h) bag.Chains with
            | Some chain ->
                let updatedChain = { chain with Elements = item :: chain.Elements }
                updatedChain :: List.filter (fun c -> c.Hash <> h) bag.Chains
            | None -> 
                { Hash = h; Elements = [item] } :: bag.Chains

        let mergedBag =
            bag2.Chains
            |> List.fold (fun acc chain -> 
                chain.Elements
                |> List.fold (fun b e -> { b with Chains = addItemToBag b e }) acc) bag1

        let filteredBag = 
            { mergedBag with Chains = List.filter (fun chain -> not (List.isEmpty chain.Elements)) mergedBag.Chains }

        if List.isEmpty filteredBag.Chains then
            { Chains = [] }
        else
            sortBag filteredBag
```

### Тест для проверки свойств моноида
```fsharp
    [<Test>]
    member _.``Merge with empty bag should return the other bag property test``() =
        let property (elements: int list) =
            let bag = elements |> List.fold (fun acc e -> add e acc) empty<int>
            let mergedBagWithEmpty1 = merge bag empty<int>
            let mergedBagWithEmpty2 = merge empty<int> bag
            bag = mergedBagWithEmpty1 && bag = mergedBagWithEmpty2

        Check.Quick property
       [<Test>]
    member _.``Merge operation should be associative property test``() =
        let property (elements1: int list) (elements2: int list) (elements3: int list) =
            let bag1 = elements1 |> List.fold (fun acc e -> add e acc) empty<int>
            let bag2 = elements2 |> List.fold (fun acc e -> add e acc) empty<int>
            let bag3 = elements3 |> List.fold (fun acc e -> add e acc) empty<int>

            let merged1 = merge (merge bag1 bag2) bag3
            let merged2 = merge bag1 (merge bag2 bag3)

            merged1 = merged2

        Check.Quick property
```

## Выводы

Реализация этой лабораторной работы на F# позволила углубить понимание принципов функционального программирования, таких как неизменяемость данных, я стала лучше понимать структуры, их особенности и различия.Также стало намного проще работать с языком F#, синтаксис стал привычнее, как мне показалось, писать легкочитаемый код стало проще.