module GraphsTests

open NinetyNineProblems.Graphs
open Xunit
open ListTests

[<Fact>]
let ``Problem 66`` () =
    let actual = graphTermOfHuman "b-c f-c g-h d f-b k-f h-g"

    let expected =
        { nodes = Set.ofList [ "b"; "c"; "f"; "g"; "h"; "d"; "k" ]
          edges =
            Set.ofList
                [ "b", "c", ()
                  "c", "f", ()
                  "g", "h", ()
                  "b", "f", ()
                  "f", "k", ()
                  "g", "h", () ] }

    assertEqualStr expected actual

[<Fact>]
let ``Problem 67`` () =
    let graph =
        { nodes = Set.ofList [ "b"; "c"; "f"; "g"; "h"; "d"; "k" ]
          edges = Set.ofList [ "b", "c", (); "c", "f", (); "b", "f", (); "f", "k", (); "g", "h", () ] }

    let actual = paths graph "f" "b"
    let expected = [ [ "f"; "c"; "b" ]; [ "f"; "b" ] ]
    assertEqualStr expected actual

[<Fact>]
let ``Problem 68`` () =
    let graph =
        { nodes = Set.ofList [ "b"; "c"; "d"; "f"; "g"; "h"; "k" ]
          edges = Set.ofList [ "b", "c", (); "c", "f", (); "b", "f", (); "f", "k", (); "g", "h", () ] }

    let actual = cycles graph "f"

    let expected =
        [ [ "f"; "c"; "b"; "f" ]
          [ "f"; "b"; "f" ]
          [ "f"; "b"; "c"; "f" ]
          [ "f"; "c"; "f" ]
          [ "f"; "k"; "f" ] ]

    assertEqualStr expected actual

[<Fact>]
let ``Problem 69`` () =
    let graph =
        { nodes = Set.ofList [ 'a'; 'b'; 'c'; 'd'; 'e'; 'f'; 'g'; 'h' ]
          edges =
            Set.ofList
                [ ('a', 'b', ())
                  ('a', 'd', ())
                  ('b', 'c', ())
                  ('b', 'e', ())
                  ('c', 'e', ())
                  ('d', 'e', ())
                  ('d', 'f', ())
                  ('d', 'g', ())
                  ('e', 'h', ())
                  ('f', 'g', ())
                  ('g', 'h', ()) ] }

    let actual = sTree graph |> List.length
    let expected = 12
    Assert.Equal(expected, actual)

[<Fact>]
let ``Problem 70`` () =
    let graph =
        { nodes = Set.ofList [ 'a'; 'b'; 'c'; 'd'; 'e'; 'f'; 'g'; 'h' ]
          edges =
            Set.ofList
                [ ('a', 'b', 5)
                  ('a', 'd', 3)
                  ('b', 'c', 2)
                  ('b', 'e', 4)
                  ('c', 'e', 6)
                  ('d', 'e', 7)
                  ('d', 'f', 4)
                  ('d', 'g', 3)
                  ('e', 'h', 5)
                  ('f', 'g', 4)
                  ('g', 'h', 1) ] }

    let actual = totalWeight (msTree graph)
    let expected = 30
    Assert.Equal(expected, actual)
