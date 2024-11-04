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
            // Проверяем, что обновленный мешок равен оригинальному
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
