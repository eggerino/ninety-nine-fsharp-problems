namespace NinetyNineProblems.Test

module MiscTests =

    open NinetyNineProblems.Misc
    open Xunit
    open Asserter

    [<Fact>]
    let ``Problem 77`` () =
        let actual = EightQueensProblem.queensPositions 4
        let expected = [ [ 3; 1; 4; 2 ]; [ 2; 4; 1; 3 ] ]
        listEqual (listEqual intEqual) expected actual

    [<Fact>]
    let ``Problem 78`` () =
        let actual = KnightsTour.jump 2 (1, 1) |> List.length
        let expected = 1
        intEqual expected actual

        let actual = KnightsTour.jump 3 (1, 1) |> List.length
        let expected = 8
        intEqual expected actual

        let actual = KnightsTour.jump 3 (2, 2) |> List.length
        let expected = 1
        intEqual expected actual

        let actual = KnightsTour.jump 8 (1, 1) |> List.length
        let expected = 64
        intEqual expected actual

    [<Fact>]
    let ``Problem 79`` () =
        let tree =
            [ 'a'; 'b'; 'c'; 'd'; 'e'; 'f'; 'g' ], [ 'd', 'a'; 'a', 'g'; 'a', 'b'; 'b', 'c'; 'b', 'e'; 'e', 'f' ]

        let actualNodes, actualEdges = VonKochsConjecture.label tree
        let expectedNodes = [ 'a', 1; 'b', 5; 'c', 2; 'd', 6; 'e', 3; 'f', 4; 'g', 7 ]

        let expectedEdges =
            [ 'd', 'a', 5; 'a', 'g', 6; 'a', 'b', 4; 'b', 'c', 3; 'b', 'e', 2; 'e', 'f', 1 ]

        listEqual (tupleEqual charEqual intEqual) expectedNodes actualNodes
        listEqual (tupleEqual3 charEqual charEqual intEqual) expectedEdges actualEdges

    [<Fact>]
    let ``Problem 80`` () =
        let actual = AritmeticPuzzle.solve [ 2; 3; 5; 7; 11 ]
        let expected = "2 = (((3) - 5) - 7) + 11"
        strEqual expected actual

    [<Fact>]
    let ``Problem 81`` () =
        let actual = EnglishNumberWords.fullWords 175
        let expected = "one-seven-five"
        strEqual expected actual

    [<Fact>]
    let ``Problem 82`` () =
        Assert.True(SyntaxChecker.identifier "a")
        Assert.False(SyntaxChecker.identifier "1")
        Assert.False(SyntaxChecker.identifier "-")
        Assert.True(SyntaxChecker.identifier "abcded")
        Assert.True(SyntaxChecker.identifier "a123")
        Assert.True(SyntaxChecker.identifier "aasdf213adas")
        Assert.True(SyntaxChecker.identifier "a-adsf2fa-fd-87")
        Assert.False(SyntaxChecker.identifier "a--fasf87")
        Assert.False(SyntaxChecker.identifier "afasf87-")
        Assert.True(SyntaxChecker.identifier "this-is-a-long-identifier")

    [<Fact>]
    let ``Problem 83`` () =
        let unsolved =
            "..48...17\n"
            + "67.9.....\n"
            + "5.8.3...4\n"
            + "3..74.1..\n"
            + ".69...78.\n"
            + "..1.69..5\n"
            + "1...8.3.6\n"
            + ".....6.91\n"
            + "24...15.."

        let solved =
            "934825617\n"
            + "672914853\n"
            + "518637924\n"
            + "325748169\n"
            + "469153782\n"
            + "781269435\n"
            + "197582346\n"
            + "853476291\n"
            + "246391578"

        let actual = Sudoku.parse unsolved
        Assert.True(Sudoku.solve (ref actual))
        let expected = Sudoku.parse solved
        arrayEqual (arrayEqual intEqual) expected actual

    [<Fact>]
    let ``Problem 84`` () =
        let rows = [ [ 3 ]; [ 1 ]; [ 1; 1 ]; [ 1 ] ]
        let cols = [ [ 1 ]; [ 1; 1 ]; [ 1; 1 ]; [ 2 ] ]

        let actual = Nanograms.solve rows cols |> Option.map Nanograms.render
        let expected = " xxx\n" + "   x\n" + "x x \n" + " x  "

        Assert.True actual.IsSome
        strEqual expected (Option.get actual)

    [<Fact>]
    let ``Problem 85`` () =
        let puzzle =
            "prolog\n"
            + "perl\n"
            + "online\n"
            + "gnu\n"
            + "linux\n"
            + "web\n"
            + "nfs\n"
            + "xml\n"
            + "sql\n"
            + "mac\n"
            + "emacs\n"
            + "\n"
            + "pr.log  e\n"
            + ". .  .  m\n"
            + "r l.nu. a\n"
            + "l i f ma.\n"
            + "  n .ql s\n"
            + " w.b     "

        let expected =
            "prolog  e\n"
            + "e n  n  m\n"
            + "r linux a\n"
            + "l i f mac\n"
            + "  n sql s\n"
            + " web     "

        let actual = CrosswordPuzzle.solve puzzle

        match actual with
        | Error(err) -> Assert.Fail $"Failed with {err}"
        | Ok(actual) -> strEqual expected actual

    [<Fact>]
    let ``Problem 86`` () =
        let stream = NeverEndingSequence.ints 5

        let actual = NeverEndingSequence.hd stream
        let expected = 5
        intEqual expected actual

        let actual = NeverEndingSequence.hd (NeverEndingSequence.tl stream)
        let expected = 6
        intEqual expected actual

        let actual = NeverEndingSequence.take 5 stream
        let expected = [ 5; 6; 7; 8; 9 ]
        listEqual intEqual expected actual

        let stream = NeverEndingSequence.unfold (fun x -> x, x + 1) 0
        let actual = NeverEndingSequence.take 5 stream
        let expected = [ 0; 1; 2; 3; 4 ]
        listEqual intEqual expected actual

        let stream = NeverEndingSequence.bang 420
        let actual = NeverEndingSequence.take 3 stream
        let expected = [ 420; 420; 420 ]
        listEqual intEqual expected actual

        let stream = NeverEndingSequence.ints 5
        let stream = NeverEndingSequence.map (fun x -> x * 2) stream
        let actual = NeverEndingSequence.take 3 stream
        let expected = [ 10; 12; 14 ]
        listEqual intEqual expected actual

        let stream = NeverEndingSequence.ints 5
        let stream = NeverEndingSequence.filter (fun x -> x % 2 = 0) stream
        let actual = NeverEndingSequence.take 3 stream
        let expected = [ 6; 8; 10 ]
        listEqual intEqual expected actual

    [<Fact>]
    let ``Problem 87`` () =
        let actual = DiagnonalOfSequenceOfSequences.diag [ [ 1 ] ] |> Seq.toList
        let expected = [ 1 ]
        listEqual intEqual expected actual

        let actual =
            DiagnonalOfSequenceOfSequences.diag [ [ 1; 2; 3 ]; [ 4; 5; 6 ]; [ 7; 8; 9 ] ]
            |> Seq.toList

        let expected = [ 1; 5; 9 ]
        listEqual intEqual expected actual


        let actual =
            DiagnonalOfSequenceOfSequences.diag [ [ 1; 2; 3 ]; []; [ 7; 8; 9 ] ]
            |> Seq.toList

        let expected = [ 1; 9 ]
        listEqual intEqual expected actual


        let actual =
            DiagnonalOfSequenceOfSequences.diag [ [ 1; 2; 3 ]; [ 4; 5; 6 ]; [ 7; 8; 9 ]; [ 10 ] ]
            |> Seq.toList

        let expected = [ 1; 5; 9 ]
        listEqual intEqual expected actual
