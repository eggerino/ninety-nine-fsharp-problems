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

[<Fact>]
let ``Problem 71`` () =
    let graph1 =
        { nodes = Set.ofList [ 1; 2; 3; 4; 5 ]
          edges = Set.ofList [ (1, 2, 1); (2, 3, 2); (3, 4, 4); (1, 4, 5); (1, 5, 6); (4, 5, 7) ] }

    let graph2 =
        { nodes = Set.ofList [ 'a'; 'b'; 'c'; 'd'; 'e' ]
          edges =
            Set.ofList
                [ ('b', 'c', 5)
                  ('a', 'c', 3)
                  ('a', 'b', 67)
                  ('b', 'e', 4)
                  ('d', 'e', 5)
                  ('c', 'd', 7) ] }

    let actual = isIsomorphic graph1 graph2
    Assert.True actual

    let graph3 =
        { nodes = Set.ofList [ 1; 2; 3; 4; 5 ]
          edges = Set.ofList [ (1, 2, 1); (2, 3, 2); (3, 4, 4); (1, 4, 5); (1, 5, 6); (4, 5, 7) ] }

    let graph4 =
        { nodes = Set.ofList [ 'a'; 'b'; 'c'; 'd'; 'e' ]
          edges =
            Set.ofList
                [ ('b', 'c', 5)
                  ('a', 'c', 3)
                  ('a', 'b', 67)
                  ('b', 'e', 4)
                  ('d', 'e', 5)
                  ('b', 'd', 7) ] }

    let actual = isIsomorphic graph3 graph4
    Assert.False actual

[<Fact>]
let ``Problem 72`` () =
    let graph =
        { nodes = Set.ofList [ 1; 2; 3; 4; 5 ]
          edges = Set.ofList [ (1, 2, 1); (2, 3, 2); (3, 4, 4); (1, 4, 5); (1, 5, 6); (4, 5, 7) ] }

    let actual = degree graph 1
    let expected = 3
    Assert.Equal(expected, actual)

    let actual = sortedNodes graph
    let expected = [ 1; 4; 2; 3; 5 ]
    assertEqualStr expected actual
