namespace Bagg

module Program =

    open System
    open Bag

    [<EntryPoint>]
    let main argv =
        // Создаем новый мешок
        let bag = createBag ()

        // Добавляем элементы в мешок
        let bagWithElements = addToBag (addToBag (addToBag bag 1 42) 1 42) 2 84
        printfn "Bag after adding elements: %A" bagWithElements

        // Удаляем один элемент из мешка
        let updatedBag = removeOneFromBag bagWithElements 1 42
        printfn "Bag after removing one element: %A" updatedBag

        // Фильтруем мешок
        let filteredBag = filterBag updatedBag (fun x -> x % 2 = 0)
        printfn "Filtered bag (even numbers): %A" filteredBag

        // Объединяем два мешка
        let bag1 = addToBag (createBag ()) 1 42
        let bag2 = addToBag (createBag ()) 2 84
        let mergedBag = mergeBags bag1 bag2
        printfn "Merged bag: %A" mergedBag

        // Используем foldLeft
        let sum = foldLeft mergedBag (+) 0
        printfn "Sum of all elements in merged bag: %d" sum

        // Завершаем программу
        0 // Возвращаем код завершения
