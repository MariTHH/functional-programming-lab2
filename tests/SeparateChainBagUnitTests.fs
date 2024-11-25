namespace Bagg.Tests

open Bagg.SeparateChainBag
open NUnit.Framework

[<TestFixture>]
type BagTests() =

    [<Test>]
    member _.``Add element to empty bag should contain the element``() =
        let bag = empty<int>
        let updatedBag = add 1 bag
        let found = find 1 updatedBag
        Assert.AreEqual(Some 1, found)

    [<Test>]
    member _.``Add multiple elements should contain all elements``() =
        let bag = empty<int>
        let updatedBag = [ 1; 2; 3 ] |> List.fold (fun acc e -> add e acc) bag
        Assert.AreEqual(Some 1, find 1 updatedBag)
        Assert.AreEqual(Some 2, find 2 updatedBag)
        Assert.AreEqual(Some 3, find 3 updatedBag)

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

    [<Test>]
    member _.``Filter should only include matching elements``() =
        let bag = [ 1; 2; 3; 4 ] |> List.fold (fun acc e -> add e acc) empty<int>
        let filteredBag = filter (fun x -> x % 2 = 0) bag
        Assert.AreEqual(Some 2, find 2 filteredBag)
        Assert.AreEqual(Some 4, find 4 filteredBag)
        Assert.AreEqual(None, find 1 filteredBag)
        Assert.AreEqual(None, find 3 filteredBag)
