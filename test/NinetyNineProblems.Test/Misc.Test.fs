module MiscTests

open NinetyNineProblems.Misc
open Xunit

let rec assertEqualList asserter l1 l2 =
    match l1, l2 with
    | [], [] -> ()
    | h1 :: t1, h2 :: t2 ->
        asserter h1 h2
        assertEqualList asserter t1 t2
    | _, _ -> Assert.Fail "Different lengths"

let assertEqualInt (a: int) b = Assert.Equal(a, b)

[<Fact>]
let ``Problem 77`` () =
    let actual = EightQueensProblem.queensPositions 4
    let expected = [ [ 3; 1; 4; 2 ]; [ 2; 4; 1; 3 ] ]

    assertEqualList (assertEqualList assertEqualInt) expected actual
