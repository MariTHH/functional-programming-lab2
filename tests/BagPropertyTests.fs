module Bagg.BagPropertyTests

open NUnit.Framework
open FsCheck
open Bagg.Bag

[<TestFixture>]
type BagTests() =

    [<Test>]
    member this.``Merging an empty bag with any bag returns the original bag``() =
        let property (bag: Bag<int>) =
            let empty = createBag ()
            mergeBags empty bag = bag && mergeBags bag empty = bag

        Check.QuickThrowOnFailure property

    [<Test>]
    member this.``Adding and removing an element maintains the original bag``() =
        let property (key: int, element: int, bag: Bag<int>) =
            let newBag = addToBag bag key element
            let updatedBag = removeOneFromBag newBag key element

            updatedBag = if List.contains element (Map.find key newBag) then
                             removeOneFromBag newBag key element
                         else
                             newBag

        Check.QuickThrowOnFailure property

    [<Test>]
    member this.``Adding an element twice keeps it twice in the bag``() =
        let property (key: int, element: int, bag: Bag<int>) =
            let newBag = addToBag (addToBag bag key element) key element
            let expected = addToBag (addToBag bag key element) key element
            expected = newBag

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
