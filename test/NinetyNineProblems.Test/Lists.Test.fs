namespace NinetyNineProblems.Test

module ListTests =

    open NinetyNineProblems.Lists
    open Xunit
    open Asserter

    let encodeDefRleEqual equal expected actual =
        match expected, actual with
        | encodeDef.One e, encodeDef.One a -> equal e a
        | encodeDef.Many(ei, e), encodeDef.Many(ai, a) ->
            intEqual ei ai
            equal e a
        | _ -> fail "Different shapes"

    [<Fact>]
    let ``Problem 1`` () =
        let actual = last [ "a"; "b"; "c"; "d" ]
        let expected = Some "d"
        optionEqual strEqual expected actual

        let actual = last ([]: int list)
        let expected = None
        optionEqual intEqual expected, actual

    [<Fact>]
    let ``Problem 2`` () =
        let actual = lastTwo [ "a"; "b"; "c"; "d" ]
        let expected = Some("c", "d")
        optionEqual (tupleEqual strEqual strEqual) expected actual

        let actual = lastTwo [ "a" ]
        let expected = None
        optionEqual (tupleEqual strEqual strEqual) expected actual

    [<Fact>]
    let ``Problem 3`` () =
        let actual = at 2 [ "a"; "b"; "c"; "d" ]
        let expected = Some "c"
        optionEqual strEqual expected actual

        let actual = at 2 [ "a" ]
        let expected = None
        optionEqual strEqual expected actual

    [<Fact>]
    let ``Problem 4`` () =
        let actual = length [ "a"; "b"; "c" ]
        let expected = 3
        intEqual expected actual

        let actual = length []
        let expected = 0
        intEqual expected actual

    [<Fact>]
    let ``Problem 5`` () =
        let expected = [ "c"; "b"; "a" ]
        let actual = rev [ "a"; "b"; "c" ]
        listEqual strEqual expected actual

    [<Fact>]
    let ``Problem 6`` () =
        let actual = isPalindrome [ "x"; "a"; "m"; "a"; "x" ]
        isTrue actual

        let actual = isPalindrome [ "a"; "b" ]
        isFalse actual

    [<Fact>]
    let ``Problem 7`` () =
        let actual =
            flatten
                [ flattenDef.One "a"
                  flattenDef.Many
                      [ flattenDef.One "b"
                        flattenDef.Many [ flattenDef.One "c"; flattenDef.One "d" ]
                        flattenDef.One "e" ] ]

        let expected = [ "a"; "b"; "c"; "d"; "e" ]
        listEqual strEqual expected actual

    [<Fact>]
    let ``Problem 8`` () =
        let actual =
            compress [ "a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e"; "e" ]

        let expected = [ "a"; "b"; "c"; "a"; "d"; "e" ]
        listEqual strEqual expected actual

    [<Fact>]
    let ``Problem 9`` () =
        let actual =
            pack [ "a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "d"; "e"; "e"; "e"; "e" ]

        let expected =
            [ [ "a"; "a"; "a"; "a" ]
              [ "b" ]
              [ "c"; "c" ]
              [ "a"; "a" ]
              [ "d"; "d" ]
              [ "e"; "e"; "e"; "e" ] ]

        listEqual (listEqual strEqual) expected actual

    [<Fact>]
    let ``Problem 10`` () =
        let actual =
            encode [ "a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e"; "e" ]

        let expected = [ 4, "a"; 1, "b"; 2, "c"; 2, "a"; 1, "d"; 4, "e" ]
        listEqual (tupleEqual intEqual strEqual) expected actual

    [<Fact>]
    let ``Problem 11`` () =
        let actual =
            encode2 [ "a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e"; "e" ]

        let expected =
            [ encodeDef.Many(4, "a")
              encodeDef.One "b"
              encodeDef.Many(2, "c")
              encodeDef.Many(2, "a")
              encodeDef.One "d"
              encodeDef.Many(4, "e") ]

        listEqual (encodeDefRleEqual strEqual) expected actual

    [<Fact>]
    let ``Problem 12`` () =
        let actual =
            decode
                [ encodeDef.Many(4, "a")
                  encodeDef.One "b"
                  encodeDef.Many(2, "c")
                  encodeDef.Many(2, "a")
                  encodeDef.One "d"
                  encodeDef.Many(4, "e") ]

        let expected =
            [ "a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e"; "e" ]

        listEqual strEqual expected actual

    [<Fact>]
    let ``Problem 13`` () =
        // Already done in the solution of problem 12
        ``Problem 12`` ()

    [<Fact>]
    let ``Problem 14`` () =
        let actual = duplicate [ "a"; "b"; "c"; "c"; "d" ]
        let expected = [ "a"; "a"; "b"; "b"; "c"; "c"; "c"; "c"; "d"; "d" ]
        listEqual strEqual expected actual

    [<Fact>]
    let ``Problem 15`` () =
        let actual = replicate [ "a"; "b"; "c" ] 3
        let expected = [ "a"; "a"; "a"; "b"; "b"; "b"; "c"; "c"; "c" ]
        listEqual strEqual expected actual

    [<Fact>]
    let ``Problem 16`` () =
        let actual = drop [ "a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"; "i"; "j" ] 3
        let expected = [ "a"; "b"; "d"; "e"; "g"; "h"; "j" ]
        listEqual strEqual expected actual

    [<Fact>]
    let ``Problem 17`` () =
        let actual = split [ "a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"; "i"; "j" ] 3
        let expected = [ "a"; "b"; "c" ], [ "d"; "e"; "f"; "g"; "h"; "i"; "j" ]
        tupleEqual (listEqual strEqual) (listEqual strEqual) expected actual

        let actual = split [ "a"; "b"; "c"; "d" ] 5
        let expected = [ "a"; "b"; "c"; "d" ], []
        tupleEqual (listEqual strEqual) (listEqual strEqual) expected actual

    [<Fact>]
    let ``Problem 18`` () =
        let actual = slice [ "a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"; "i"; "j" ] 2 6
        let expected = [ "c"; "d"; "e"; "f"; "g" ]
        listEqual strEqual expected actual

    [<Fact>]
    let ``Problem 19`` () =
        let actual = rotate [ "a"; "b"; "c"; "d"; "e"; "f"; "g"; "h" ] 3
        let expected = [ "d"; "e"; "f"; "g"; "h"; "a"; "b"; "c" ]
        listEqual strEqual expected actual

    [<Fact>]
    let ``Problem 20`` () =
        let actual = removeAt 1 [ "a"; "b"; "c"; "d" ]
        let expected = [ "a"; "c"; "d" ]
        listEqual strEqual expected actual

    [<Fact>]
    let ``Problem 21`` () =
        let actual = insertAt "alfa" 1 [ "a"; "b"; "c"; "d" ]
        let expected = [ "a"; "alfa"; "b"; "c"; "d" ]
        listEqual strEqual expected actual

    [<Fact>]
    let ``Problem 22`` () =
        let actual = range 4 9
        let expected = [ 4; 5; 6; 7; 8; 9 ]
        listEqual intEqual expected actual

    [<Fact>]
    let ``Problem 23`` () =
        (* 
        Random!!
        let actual = randSelect [ "a"; "b"; "c"; "d"; "e"; "f"; "g"; "h" ] 3
        let expected = [ "e"; "c"; "g" ]
        listEqual strEqual expected actual
        *)
        ()


    [<Fact>]
    let ``Problem 24`` () =
        (*
        Random!!
        let actual = lottoSelect 6 49
        let expected = [ 20; 28; 45; 16; 24; 38 ]
        listEqual strEqual expected actual
        *)
        ()

    [<Fact>]
    let ``Problem 25`` () =
        (*
        Random!!
        let actual = permutation [ "a"; "b"; "c"; "d"; "e"; "f" ]
        let expected = [ "c"; "d"; "f"; "e"; "b"; "a" ]
        listEqual strEqual expected actual
        *)
        ()

    [<Fact>]
    let ``Problem 26`` () =
        let actual = extract 2 [ "a"; "b"; "c"; "d" ]

        let expected =
            [ [ "a"; "b" ]
              [ "a"; "c" ]
              [ "a"; "d" ]
              [ "b"; "c" ]
              [ "b"; "d" ]
              [ "c"; "d" ] ]

        listEqual (listEqual strEqual) expected actual

    [<Fact>]
    let ``Problem 27`` () =
        let actual = group [ "a"; "b"; "c"; "d" ] [ 2; 1 ]

        let expected =
            [ [ [ "a"; "b" ]; [ "c" ] ]
              [ [ "a"; "c" ]; [ "b" ] ]
              [ [ "b"; "c" ]; [ "a" ] ]
              [ [ "a"; "b" ]; [ "d" ] ]
              [ [ "a"; "c" ]; [ "d" ] ]
              [ [ "b"; "c" ]; [ "d" ] ]
              [ [ "a"; "d" ]; [ "b" ] ]
              [ [ "b"; "d" ]; [ "a" ] ]
              [ [ "a"; "d" ]; [ "c" ] ]
              [ [ "b"; "d" ]; [ "c" ] ]
              [ [ "c"; "d" ]; [ "a" ] ]
              [ [ "c"; "d" ]; [ "b" ] ] ]

        listEqual (listEqual (listEqual strEqual)) expected actual

    [<Fact>]
    let ``Problem 28`` () =
        let actual =
            lengthSort
                [ [ "a"; "b"; "c" ]
                  [ "d"; "e" ]
                  [ "f"; "g"; "h" ]
                  [ "d"; "e" ]
                  [ "i"; "j"; "k"; "l" ]
                  [ "m"; "n" ]
                  [ "o" ] ]

        let expected =
            [ [ "o" ]
              [ "d"; "e" ]
              [ "d"; "e" ]
              [ "m"; "n" ]
              [ "a"; "b"; "c" ]
              [ "f"; "g"; "h" ]
              [ "i"; "j"; "k"; "l" ] ]

        listEqual (listEqual strEqual) expected actual

        let actual =
            frequencySort
                [ [ "a"; "b"; "c" ]
                  [ "d"; "e" ]
                  [ "f"; "g"; "h" ]
                  [ "d"; "e" ]
                  [ "i"; "j"; "k"; "l" ]
                  [ "m"; "n" ]
                  [ "o" ] ]

        let expected =
            [ [ "i"; "j"; "k"; "l" ]
              [ "o" ]
              [ "a"; "b"; "c" ]
              [ "f"; "g"; "h" ]
              [ "d"; "e" ]
              [ "d"; "e" ]
              [ "m"; "n" ] ]

        listEqual (listEqual strEqual) expected actual
