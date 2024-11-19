namespace Bagg

module Program =

    open System
    open SeparateChainBag

    [<EntryPoint>]
     let main argv =
        let intBag = empty<int>
        printfn "Создан пустой Bag для int: %A" intBag

        let intBag = intBag |> add 1 |> add 2 |> add 3 |> add 1 |> add 3
        printfn "Bag после добавления элементов (1, 2, 3, 1, 3): %A" intBag

        let intBag = intBag |> remove 1
        printfn "Bag после удаления элемента 1: %A" intBag

        let filteredIntBag = intBag |> filter (fun x -> x % 2 = 0)
        printfn "Bag после фильтрации (оставлены только четные числа): %A" filteredIntBag

        let mappedIntBag = intBag |> map (fun x -> x * 10)
        printfn "Bag после отображения (умножение каждого элемента на 10): %A" mappedIntBag

        let sum = intBag |> foldLeft (+) 0
        printfn "Сумма всех элементов Bag для int: %d" sum

        let bag2 = empty<int> |> add 4 |> add 5
        let mergedIntBag = merge intBag bag2
        printfn "Объединение двух Bag для int: %A" mergedIntBag

        let stringBag = empty<string>
        let stringBag = stringBag |> add "apple" |> add "banana" |> add "cherry" |> add "banana"
        printfn "Bag для строк: %A" stringBag

        let filteredStringBag = stringBag |> filter (fun s -> s.StartsWith("b"))
        printfn "Bag для строк, начинающихся с 'b': %A" filteredStringBag

        let mappedStringBag = stringBag |> map (fun s -> s.ToUpper())
        printfn "Bag после преобразования строк в верхний регистр: %A" mappedStringBag

        let intBag = empty<int>
        printfn "Создан пустой Bag для int: %A" intBag

        let intBag = intBag |> add 1 |> add 2 |> add 3 |> add 1 |> add 3
        printfn "Bag после добавления элементов (1, 2, 3, 1, 3): %A" intBag

        let intBagAfterRemoveOne = intBag |> remove 1
        printfn "Bag после удаления одного экземпляра элемента 1: %A" intBagAfterRemoveOne

        let intBagAfterRemoveAll = intBag |> removeAll 3
        printfn "Bag после удаления всех экземпляров элемента 3: %A" intBagAfterRemoveAll

        let conflictingBag = empty<int> |> add 2 |> add -2
        printfn "Bag с элементами 2 и -2, которые могут иметь одинаковые хэши: %A" conflictingBag

        0
