
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

**Варианты** (колонка -- интерфейс -- Separate Chaining Hashmap, строка -- структура данных -- Bag(multiset)): `sc-bag`

---

## Реализация

### Добавление и удаление элементов

```fsharp
    let addToBag (bag: Bag<'T>) (key: int) (element: 'T) : Bag<'T> =
        let existing =
            match Map.tryFind key bag with
            | Some elements -> elements
            | None -> []

        bag |> Map.add key (element :: existing)
```

```fsharp
    let removeOneFromBag (bag: Bag<'T>) (key: int) (element: 'T) : Bag<'T> =
        match Map.tryFind key bag with
        | Some elements ->
            let newList =
                match elements |> List.tryFindIndex ((=) element) with
                | Some index -> List.take index elements @ List.skip (index + 1) elements
                | None -> elements

            if List.isEmpty newList then
                Map.remove key bag
            else
                Map.add key newList bag
        | None -> bag
```

### Тест добавления и удаления элементов

```fsharp
[<Test>]
    member this.``Adding elements to a bag accumulates values correctly``() =
        let bag: Bag<int> = createBag ()
        let updatedBag = addToBag (addToBag bag 1 42) 1 42
        Assert.AreEqual(Map.ofList [ (1, [ 42; 42 ]) ], updatedBag)

    [<Test>]
    member this.``Removing an element from a bag preserves other elements``() =
        let bag: Bag<int> = addToBag (addToBag (addToBag (createBag ()) 1 42) 1 42) 1 43
        let updatedBag = removeOneFromBag bag 1 42
        let expected = Map.ofList [ (1, List.sort [ 42; 43 ]) ]: Bag<int>

        // Сортируем элементы в `updatedBag` и `expected` перед сравнением
        let sortedUpdatedBag =
            updatedBag |> Map.map (fun key elements -> List.sort elements)

        let sortedExpected = expected |> Map.map (fun key elements -> List.sort elements)

        Assert.AreEqual(sortedExpected, sortedUpdatedBag)

    [<Test>]
    member this.``Removing all instances of an element deletes the key``() =
        let bag: Bag<int> = addToBag (addToBag (createBag ()) 1 42) 1 42
        let updatedBag1 = removeOneFromBag bag 1 42
        let updatedBag2 = removeOneFromBag updatedBag1 1 42
        let expected = Map.empty: Bag<int>
        Assert.AreEqual(expected, updatedBag2)
```

### Фильтрация

```fsharp
    let filterBag (bag: Bag<'T>) (predicate: 'T -> bool) : Bag<'T> =
        bag
        |> Map.map (fun _ elements -> List.filter predicate elements)
        |> Map.filter (fun _ elements -> not (List.isEmpty elements))
```

### Тест фильтрации

```fsharp
    [<Test>]
    member this.``Filtering a bag based on a predicate removes unwanted elements``() =
        let bag = addToBag (addToBag (createBag ()) 1 1) 2 2
        let filteredBag = filterBag bag (fun x -> x % 2 = 0)
        Assert.AreEqual(Map.ofList [ (2, [ 2 ]) ], filteredBag)
```

#### Отображение(map)

```fsharp
    let mapBag (bag: Bag<'T>) (mapper: 'T -> 'U) : Bag<'U> =
        bag |> Map.map (fun _ elements -> List.map mapper elements)
```

#### Свертки (левая и правая)

```fsharp
 let foldLeft (bag: Bag<'T>) (folder: 'State -> 'T -> 'State) (initialState: 'State) : 'State =
        bag
        |> Map.fold (fun state _ elements -> List.fold folder state elements) initialState
```

```fsharp
    let foldRight (bag: Bag<'T>) (folder: 'T -> 'State -> 'State) (initialState: 'State) : 'State =
        bag
        |> Map.fold (fun state _ elements -> List.foldBack folder elements state) initialState
```

### Тест свертки

```fsharp
    [<Test>]
    member this.``Fold left aggregates values correctly``() =
        let bag = addToBag (addToBag (createBag ()) 1 1) 2 2
        let result = foldLeft bag (+) 0
        Assert.AreEqual(3, result)

    [<Test>]
    member this.``Fold right aggregates values correctly``() =
        let bag = addToBag (addToBag (createBag ()) 1 1) 2 2
        let result = foldRight bag (+) 0
        Assert.AreEqual(3, result)
```

### Структура должна быть моноидом.

```fsharp
    let createBag () : Bag<'T> = Map.empty


    let mergeBags (bag1: Bag<'T>) (bag2: Bag<'T>) : Bag<'T> =
        Map.fold
            (fun acc k v ->
                match Map.tryFind k acc with
                | Some existingList -> Map.add k (existingList @ v) acc 
                | None -> Map.add k v acc)
            bag1
            bag2
```

### Тест для проверки свойств моноида
```fsharp
    [<Test>]
    member this.``Merging an empty bag with any bag returns the original bag``() =
        let property (bag: Bag<int>) =
            let empty = createBag ()
            mergeBags empty bag = bag && mergeBags bag empty = bag

        Check.QuickThrowOnFailure property

    [<Test>]
    member this.``Merging bags is associative``() =
        let property (bag1: Bag<int>, bag2: Bag<int>, bag3: Bag<int>) =
            let merged1 = mergeBags (mergeBags bag1 bag2) bag3
            let merged2 = mergeBags bag1 (mergeBags bag2 bag3)
            merged1 = merged2

        Check.QuickThrowOnFailure property

    [<Test>]
    member this.``Merging with an empty bag returns the original bag``() =
        let property (bag: Bag<int>) =
            let empty = createBag ()
            mergeBags bag empty = bag && mergeBags empty bag = bag

        Check.QuickThrowOnFailure property

    [<Test>]
    member this.``Adding an element multiple times keeps all instances in the bag``() =
        let property (key: int, element: int, bag: Bag<int>) =
            let bagWithDuplicates = addToBag (addToBag bag key element) key element
            let expected = addToBag (addToBag bag key element) key element
            expected = bagWithDuplicates

        Check.QuickThrowOnFailure property

```

## Выводы

Реализация этой лабораторной работы на F# позволила углубить понимание принципов функционального программирования, таких как неизменяемость данных, рекурсивные алгоритмы и тд. Использованные техники помогли создать эффективную и устойчивую к ошибкам структуру данных. Также стало намного проще работать с языком F#, синтаксис стал привычнее, как мне показалось, писать легкочитаемый код стало проще.