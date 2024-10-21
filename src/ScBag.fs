module ScBag

type ScBag<'Key, 'Value when 'Key : comparison> = 
    | Empty
    | Node of 'Key * 'Value list * ScBag<'Key, 'Value>

let rec add key value (bag: ScBag<'Key, 'Value>) =
    match bag with
    | Empty -> Node(key, [value], Empty)
    | Node(k, values, next) when k = key -> Node(k, value :: values, next)
    | Node(k, values, next) -> Node(k, values, add key value next)

let rec remove key value (bag: ScBag<'Key, 'Value>) =
    match bag with
    | Empty -> Empty
    | Node(k, values, next) when k = key ->
        let updatedValues = List.filter (fun v -> v <> value) values
        if List.isEmpty updatedValues then next
        else Node(k, updatedValues, next)
    | Node(k, values, next) -> Node(k, values, remove key value next)

let rec filter pred (bag: ScBag<'Key, 'Value>) =
    match bag with
    | Empty -> Empty
    | Node(k, values, next) ->
        let filteredValues = List.filter pred values
        if List.isEmpty filteredValues then filter pred next
        else Node(k, filteredValues, filter pred next)

let rec map f (bag: ScBag<'Key, 'Value>) =
    match bag with
    | Empty -> Empty
    | Node(k, values, next) -> 
        let mappedValues = List.map f values
        Node(k, mappedValues, map f next)

let rec foldLeft f acc (bag: ScBag<'Key, 'Value>) =
    match bag with
    | Empty -> acc
    | Node(_, values, next) -> 
        let newAcc = List.fold f acc values
        foldLeft f newAcc next

let rec foldRight f (bag: ScBag<'Key, 'Value>) acc =
    match bag with
    | Empty -> acc
    | Node(_, values, next) -> 
        let newAcc = List.foldBack f values acc
        foldRight f next newAcc

let mempty = Empty

let rec mappend (bag1: ScBag<'k, 'v>) (bag2: ScBag<'k, 'v>) : ScBag<'k, 'v> =
    match (bag1, bag2) with
    | (Empty, _) -> bag2
    | (_, Empty) -> bag1
    | (Node(key1, values1, next1), _) ->
        let nextBag = mappend next1 bag2
        Node(key1, values1, nextBag)



