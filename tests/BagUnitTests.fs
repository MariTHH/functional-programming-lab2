namespace Bagg.BagUnitTests

open NUnit.Framework
open Bagg.Bag

[<TestFixture>]
type BagTests() =

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

    [<Test>]
    member this.``Merging two bags combines their contents``() =
        let bag1 = addToBag (createBag ()) 1 42
        let bag2 = addToBag (createBag ()) 2 84
        let merged = mergeBags bag1 bag2
        Assert.AreEqual(Map.ofList [ (1, [ 42 ]); (2, [ 84 ]) ], merged)

    [<Test>]
    member this.``Filtering a bag based on a predicate removes unwanted elements``() =
        let bag = addToBag (addToBag (createBag ()) 1 1) 2 2
        let filteredBag = filterBag bag (fun x -> x % 2 = 0)
        Assert.AreEqual(Map.ofList [ (2, [ 2 ]) ], filteredBag)

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
