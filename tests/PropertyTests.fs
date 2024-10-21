module PropertyTests

open FsCheck
open NUnit.Framework
open ScBag

[<Test>]
let ``Monoid associativity property`` () =
    let prop (bag1: ScBag<string, int>) (bag2: ScBag<string, int>) (bag3: ScBag<string, int>) = 
        mappend (mappend bag1 bag2) bag3 = mappend bag1 (mappend bag2 bag3)
        
    Check.QuickThrowOnFailure prop

[<Test>]
let ``Add and remove property`` () =
    let prop (key: string) (value: int) (bag: ScBag<string, int>) =
        if key = "" then
            true 
        
        else
            let modifiedBag = add key value bag
            let resultBag = remove key value modifiedBag
            
            let isEqual = bag = resultBag
            
            if not isEqual then
                printfn "Ошибка: bag не совпадает с исходным bag после удаления ключа '%s' со значением '%d'." key value
            
            isEqual

    Check.QuickThrowOnFailure prop

[<Test>]
let ``Removing a non-existent key does not change the bag`` () =
    let bag = Empty |> add "a" 1
    let modifiedBag = remove "b" 2 bag 
    Assert.AreEqual(bag, modifiedBag, "Ошибка: bag изменился при удалении несуществующего ключа.")

[<Test>]
let ``Removing a value that does not exist for a key does not change the bag`` () =
    let bag = Empty |> add "a" 1 |> add "a" 2
    let modifiedBag = remove "a" 3 bag 
    Assert.AreEqual(bag, modifiedBag, "Ошибка: bag изменился при удалении несуществующего значения для существующего ключа.")

[<Test>]
let ``Filtering on an empty bag returns an empty bag`` () =
    let bag: ScBag<int, int> = Empty 
    let filteredBag = filter (fun x -> x > 1) bag
    Assert.AreEqual(bag, filteredBag, "Ошибка: Фильтрация пустого bag не возвращает пустой bag.")

[<Test>]
let ``Mapping on an empty bag returns an empty bag`` () =
    let bag: ScBag<int, int> = Empty 
    let mappedBag = map ((*) 2) bag
    Assert.AreEqual(bag, mappedBag, "Ошибка: Отображение пустого bag не возвращает пустой bag.")
