
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

**Варианты** (колонка -- интерфей -- Separate Chaining Hashmap, строка -- структура данных -- Bag(multiset)): `sc-bag`

---

## Реализация

### Добавление и удаление элементов

```fsharp
let rec add key value (bag: ScBag<'Key, 'Value>) =
    match bag with
    | Empty -> Node(key, [value], Empty)
    | Node(k, values, next) when k = key -> Node(k, value :: values, next)
    | Node(k, values, next) -> Node(k, values, add key value next)
```

```fsharp
let rec remove key value (bag: ScBag<'Key, 'Value>) =
    match bag with
    | Empty -> Empty
    | Node(k, values, next) when k = key ->
        let updatedValues = List.filter (fun v -> v <> value) values
        if List.isEmpty updatedValues then next
        else Node(k, updatedValues, next)
    | Node(k, values, next) -> Node(k, values, remove key value next)
```
### Тест добавления и удаления элементов

```fsharp
[<Test>]
let ``Add elements to sc-bag`` () =
    let bag = Empty |> add "a" 1 |> add "a" 2 |> add "b" 3
    match bag with
    | Node("a", [2; 1], Node("b", [3], Empty)) -> Assert.Pass()
    | _ -> Assert.Fail("Ошибка: Не удалось добавить элементы в sc-bag.")

[<Test>]
let ``Remove element from sc-bag`` () =
    let bag = Empty |> add "a" 1 |> add "a" 2 |> remove "a" 1
    match bag with
    | Node("a", [2], Empty) -> Assert.Pass()
    | _ -> Assert.Fail("Ошибка: Не удалось удалить элемент из sc-bag.")
```

### Фильтрация

```fsharp
let rec filter pred (bag: ScBag<'Key, 'Value>) =
    match bag with
    | Empty -> Empty
    | Node(k, values, next) ->
        let filteredValues = List.filter pred values
        if List.isEmpty filteredValues then filter pred next
        else Node(k, filteredValues, filter pred next)
```

### Тест фильтрации

```fsharp
[<Test>]
let ``Filter works correctly`` () =
    let bag = Empty |> add "a" 1 |> add "b" 2 |> add "c" 3
    let filteredBag = filter (fun x -> x > 1) bag
    match filteredBag with
    | Node("b", [2], Node("c", [3], Empty)) -> Assert.Pass()
    | _ -> Assert.Fail("Ошибка: Фильтрация не работает корректно.")
```

#### Отображение(map)

```fsharp
let rec map f (bag: ScBag<'Key, 'Value>) =
    match bag with
    | Empty -> Empty
    | Node(k, values, next) -> 
        let mappedValues = List.map f values
        Node(k, mappedValues, map f next)
```

### Тест отображения

```fsharp
[<Test>]
let ``Map works correctly`` () =
    let bag = Empty |> add "a" 1 |> add "b" 2
    let mappedBag = map ((*) 2) bag
    match mappedBag with
    | Node("a", [2], Node("b", [4], Empty)) -> Assert.Pass()
    | _ -> Assert.Fail("Ошибка: Отображение не работает корректно.")
```

#### Свертки (левая и правая)

```fsharp
let rec foldLeft f acc (bag: ScBag<'Key, 'Value>) =
    match bag with
    | Empty -> acc
    | Node(_, values, next) -> 
        let newAcc = List.fold f acc values
        foldLeft f newAcc next
```

```fsharp
let rec foldRight f (bag: ScBag<'Key, 'Value>) acc =
    match bag with
    | Empty -> acc
    | Node(_, values, next) -> 
        let newAcc = List.foldBack f values acc
        foldRight f next newAcc
```

### Тест свертки

```fsharp
[<Test>]
let ``FoldLeft works correctly`` () =
    let bag = Empty |> add "a" 1 |> add "b" 2 |> add "c" 3
    let sum = foldLeft (+) 0 bag
    Assert.AreEqual(6, sum, "Ошибка: Неверный результат свертки влево.")
```

### Структура должна быть моноидом.

```fsharp
let mempty = Empty

let rec mappend (bag1: ScBag<'k, 'v>) (bag2: ScBag<'k, 'v>) : ScBag<'k, 'v> =
    match (bag1, bag2) with
    | (Empty, _) -> bag2
    | (_, Empty) -> bag1
    | (Node(key1, values1, next1), _) ->
        let nextBag = mappend next1 bag2
        Node(key1, values1, nextBag)
```

### Тест для проверки ассоциативности
```fsharp
[<Test>]
let ``Monoid associativity property`` () =
    let prop (bag1: ScBag<string, int>) (bag2: ScBag<string, int>) (bag3: ScBag<string, int>) = 
        mappend (mappend bag1 bag2) bag3 = mappend bag1 (mappend bag2 bag3)
        
    Check.QuickThrowOnFailure prop
```

## Выводы

Реализация этой лабораторной работы на F# позволила углубить понимание принципов функционального программирования, таких как неизменяемость данных, рекурсивные алгоритмы и тд. Использованные техники помогли создать эффективную и устойчивую к ошибкам структуру данных. Также стало намного проще работать с языком F#, синтаксис стал привычнее, как мне показалось, писать легкочитаемый код стало проще.