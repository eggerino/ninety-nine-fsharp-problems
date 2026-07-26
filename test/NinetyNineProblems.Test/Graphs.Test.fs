namespace NinetyNineProblems.Test

module GraphsTests =

    open NinetyNineProblems.Graphs
    open Xunit
    open Asserter

    let graphEqual expected actual =
        setEqual expected.nodes actual.nodes
        setEqual expected.edges actual.edges

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

        graphEqual expected actual

    [<Fact>]
    let ``Problem 67`` () =
        let graph =
            { nodes = Set.ofList [ "b"; "c"; "f"; "g"; "h"; "d"; "k" ]
              edges = Set.ofList [ "b", "c", (); "c", "f", (); "b", "f", (); "f", "k", (); "g", "h", () ] }

        let actual = paths graph "f" "b"
        let expected = [ [ "f"; "c"; "b" ]; [ "f"; "b" ] ]
        listEqual (listEqual strEqual) expected actual

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

        listEqual (listEqual strEqual) expected actual

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
        intEqual expected actual

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
        intEqual expected actual

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
        isTrue actual

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
        isFalse actual

    [<Fact>]
    let ``Problem 72`` () =
        let graph =
            { nodes = Set.ofList [ 1; 2; 3; 4; 5 ]
              edges = Set.ofList [ (1, 2, 1); (2, 3, 2); (3, 4, 4); (1, 4, 5); (1, 5, 6); (4, 5, 7) ] }

        let actual = degree graph 1
        let expected = 3
        intEqual expected actual

        let actual = sortedNodes graph
        let expected = [ 1; 4; 2; 3; 5 ]
        listEqual intEqual expected actual

    [<Fact>]
    let ``Problem 73`` () =
        let graph =
            M.ofAdjacency
                [ 'u', [ 'v'; 'x' ]
                  'v', [ 'y' ]
                  'w', [ 'z'; 'y' ]
                  'x', [ 'v' ]
                  'y', [ 'x' ]
                  'z', [ 'z' ] ]

        let actual = M.dfsFold graph 'u' (fun acc n -> n :: acc) []
        let expected = [ 'z'; 'w'; 'y'; 'x'; 'v'; 'u' ]
        listEqual charEqual expected actual

    [<Fact>]
    let ``Problem 74`` () =
        let graph =
            M.ofAdjacency
                [ 'u', [ 'v'; 'x' ]
                  'v', [ 'y' ]
                  'w', [ 'z' ]
                  'x', [ 'v' ]
                  'y', [ 'x' ]
                  'z', [ 'z' ] ]

        let actual = List.length (splitConnected graph)
        let expected = 2
        intEqual expected actual

    [<Fact>]
    let ``Problem 75`` () =
        let graph =
            M.ofAdjacency
                [ 'u', [ 'v'; 'x' ]
                  'v', [ 'y' ]
                  'w', [ 'z'; 'y' ]
                  'x', [ 'v' ]
                  'y', [ 'x' ]
                  'z', [ 'z' ] ]

        let actual = isBipartite graph
        isFalse actual

        let graph =
            M.ofAdjacency
                [ 'u', [ 'v'; 'x' ]
                  'v', [ 'y'; 'w' ]
                  'w', [ 'z'; 'x' ]
                  'x', [ 'y' ]
                  'y', [ 'z' ]
                  'z', [ 'u' ] ]

        let actual = isBipartite graph
        isTrue actual

    [<Fact>]
    let ``Problem 76`` () =
        let testCases =
            [ 3, 2, 1
              4, 2, 1
              4, 3, 1
              5, 2, 1
              5, 3, 0
              5, 4, 1
              6, 2, 2
              6, 3, 2
              6, 4, 1
              6, 5, 1

              // Slow cases. All 5 took ~40s on my machine.
              // 7, 2, 2
              // 7, 3, 0
              // 7, 4, 2
              // 7, 5, 0
              // 7, 6, 1

              // Extremly slow cases. One didn't finish under 100s on my machine. Never acutally ran them
              // 8, 2, 3
              // 8, 3, 6
              // 8, 4, 6
              // 8, 5, 3
              // 8, 6, 1
              // 8, 7, 1
              // 9, 2, 4
              ]

        let testSingle _ data =
            let n, k, expected = data
            let actual = List.length (regulars k n)
            intEqual expected actual

        Seq.fold testSingle () testCases
