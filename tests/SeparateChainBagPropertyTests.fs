namespace Bagg.Tests

open Bagg.SeparateChainBag
open FsCheck
open NUnit.Framework

[<TestFixture>]
type BagPropertyTests() =

    [<Test>]
    member _.``Add and find element property test``() =
        let property (element: int) =
            let bag = empty<int>
            let updatedBag = add element bag

            match find element updatedBag with
            | Some x -> x = element
            | None -> false

        Check.Quick property

    [<Test>]
    member _.``Add, remove, and ensure element is absent property test``() =
        let property (element: int) =
            let bag = empty<int>
            let updatedBag = add element bag |> remove element

            match find element updatedBag with
            | Some _ -> false
            | None -> true

        Check.Quick property

    [<Test>]
    member _.``Filter should return only matching elements property test``() =
        let property (predicate: int -> bool) (elements: int list) =
            let bag = elements |> List.fold (fun acc e -> add e acc) empty<int>
            let filteredBag = filter predicate bag

            elements
            |> List.forall (fun e ->
                if predicate e then
                    find e filteredBag = Some e
                else
                    find e filteredBag = None)

        Check.Quick property

    [<Test>]
    member _.``Fold left should accumulate all elements property test``() =
        let property (elements: int list) =
            let bag = elements |> List.fold (fun acc e -> add e acc) empty<int>
            let sum = foldLeft (+) 0 bag
            sum = List.sum elements

        Check.Quick property

    [<Test>]
    member _.``Merge should combine elements from both bags property test``() =
        let property (elements1: int list) (elements2: int list) =
            let bag1 = elements1 |> List.fold (fun acc e -> add e acc) empty<int>
            let bag2 = elements2 |> List.fold (fun acc e -> add e acc) empty<int>
            let mergedBag = merge bag1 bag2

            elements1 |> List.forall (fun e -> find e mergedBag = Some e)
            && elements2 |> List.forall (fun e -> find e mergedBag = Some e)

        Check.Quick property

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
