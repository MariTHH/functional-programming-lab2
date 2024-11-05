namespace Bagg

module Bag =

    type Bag<'T> = Map<int, 'T list>

    let createBag () : Bag<'T> = Map.empty

    let addToBag (bag: Bag<'T>) (key: int) (element: 'T) : Bag<'T> =
        let existing =
            match Map.tryFind key bag with
            | Some elements -> elements
            | None -> []

        bag |> Map.add key (element :: existing)

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

    let filterBag (bag: Bag<'T>) (predicate: 'T -> bool) : Bag<'T> =
        bag
        |> Map.map (fun _ elements -> List.filter predicate elements)
        |> Map.filter (fun _ elements -> not (List.isEmpty elements))

    let mapBag (bag: Bag<'T>) (mapper: 'T -> 'U) : Bag<'U> =
        bag |> Map.map (fun _ elements -> List.map mapper elements)

    let foldLeft (bag: Bag<'T>) (folder: 'State -> 'T -> 'State) (initialState: 'State) : 'State =
        bag
        |> Map.fold (fun state _ elements -> List.fold folder state elements) initialState

    let foldRight (bag: Bag<'T>) (folder: 'T -> 'State -> 'State) (initialState: 'State) : 'State =
        bag
        |> Map.fold (fun state _ elements -> List.foldBack folder elements state) initialState

    let mergeBags (bag1: Bag<'T>) (bag2: Bag<'T>) : Bag<'T> =
        Map.fold
            (fun acc k v ->
                match Map.tryFind k acc with
                | Some existingList -> Map.add k (existingList @ v) acc 
                | None -> Map.add k v acc)
            bag1
            bag2
