module LogicTests

open NinetyNineProblems.Logic
open Xunit
open ListTests

[<Fact>]
let ``Problem 40`` () =
    let actual = table2 "a" "b" (And(Var "a", Or(Var "a", Var "b")))

    let expected =
        [ (true, true, true)
          (true, false, true)
          (false, true, false)
          (false, false, false) ]

    assertEqualStr expected actual

[<Fact>]
let ``Problem 41`` () =
    let actual = table [ "a"; "b" ] (And(Var "a", Or(Var "a", Var "b")))

    let expected =
        [ ([ ("a", true); ("b", true) ], true)
          ([ ("a", true); ("b", false) ], true)
          ([ ("a", false); ("b", true) ], false)
          ([ ("a", false); ("b", false) ], false) ]

    assertEqualStr expected actual

[<Fact>]
let ``Problem 42`` () =
    let actual = gray 1
    let expected = [ "0"; "1" ]
    assertEqualStr expected actual

    let actual = gray 2
    let expected = [ "00"; "01"; "10"; "11" ]
    assertEqualStr expected actual

    let actual = gray 3
    let expected = [ "000"; "001"; "010"; "011"; "110"; "111"; "101"; "100" ]
    assertEqualStr expected actual

[<Fact>]
let ``Problem 43`` () =
    let fs = [ "a", 45; "b", 13; "c", 12; "d", 16; "e", 9; "f", 5 ]
    let actual = huffman fs

    let expected =
        [ ("f", "11111")
          ("e", "11110")
          ("c", "1110")
          ("b", "110")
          ("d", "10")
          ("a", "0") ]

    assertEqualStr expected actual
