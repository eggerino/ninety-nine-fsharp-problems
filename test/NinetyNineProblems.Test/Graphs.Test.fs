module GraphsTests

open NinetyNineProblems.Graphs
open Xunit
open ListTests

[<Fact>]
let ``Problem 66`` () =
    let actual = graphTermOfHuman "b-c f-c g-h d f-b k-f h-g"

    let expected =
        { nodes = Set.ofList [ "b"; "c"; "f"; "g"; "h"; "d"; "k" ]
          edges = Set.ofList [ "b", "c"; "c", "f"; "g", "h"; "b", "f"; "f", "k"; "g", "h" ] }

    assertEqualStr expected actual

[<Fact>]
let ``Problem 67`` () =
    let graph =
        { nodes = Set.ofList [ "b"; "c"; "f"; "g"; "h"; "d"; "k" ]
          edges = Set.ofList [ "b", "c"; "c", "f"; "b", "f"; "f", "k"; "g", "h" ] }

    let actual = paths graph "f" "b"
    let expected = [ [ "f"; "c"; "b" ]; [ "f"; "b" ] ]
    assertEqualStr expected actual

[<Fact>]
let ``Problem 68`` () =
    let graph =
        { nodes = Set.ofList [ "b"; "c"; "d"; "f"; "g"; "h"; "k" ]
          edges = Set.ofList [ "b", "c"; "c", "f"; "b", "f"; "f", "k"; "g", "h" ] }

    let actual = cycles graph "f"

    let expected =
        [ [ "f"; "c"; "b"; "f" ]
          [ "f"; "b"; "f" ]
          [ "f"; "b"; "c"; "f" ]
          [ "f"; "c"; "f" ]
          [ "f"; "k"; "f" ] ]

    assertEqualStr expected actual
