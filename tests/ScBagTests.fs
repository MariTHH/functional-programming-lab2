module ScBagTests

open NUnit.Framework
open ScBag

[<Test>]
let ``Add elements to sc-bag`` () =
    let bag = Empty |> add "a" 1 |> add "a" 2 |> add "b" 3
    match bag with
    | Node("a", [2; 1], Node("b", [3], Empty)) -> Assert.Pass()
    | _ -> Assert.Fail("Ошибка: Не удалось добавить элементы в sc-bag.")

[<Test>]
let ``Remove element from sc-bag`` () =
    let bag = Empty |> add "a" 1 |> add "a" 2 |> remove "a" 1
    match bag with
    | Node("a", [2], Empty) -> Assert.Pass()
    | _ -> Assert.Fail("Ошибка: Не удалось удалить элемент из sc-bag.")

[<Test>]
let ``FoldLeft works correctly`` () =
    let bag = Empty |> add "a" 1 |> add "b" 2 |> add "c" 3
    let sum = foldLeft (+) 0 bag
    Assert.AreEqual(6, sum, "Ошибка: Неверный результат свертки влево.")

[<Test>]
let ``Filter works correctly`` () =
    let bag = Empty |> add "a" 1 |> add "b" 2 |> add "c" 3
    let filteredBag = filter (fun x -> x > 1) bag
    match filteredBag with
    | Node("b", [2], Node("c", [3], Empty)) -> Assert.Pass()
    | _ -> Assert.Fail("Ошибка: Фильтрация не работает корректно.")

[<Test>]
let ``Map works correctly`` () =
    let bag = Empty |> add "a" 1 |> add "b" 2
    let mappedBag = map ((*) 2) bag
    match mappedBag with
    | Node("a", [2], Node("b", [4], Empty)) -> Assert.Pass()
    | _ -> Assert.Fail("Ошибка: Отображение не работает корректно.")
