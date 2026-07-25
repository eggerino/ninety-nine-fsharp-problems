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

let rec assertEqualArray asserter l1 l2 =
    Assert.Equal(Array.length l1, Array.length l2)
    let _ = Seq.zip l1 l2 |> Seq.map (fun (x1, x2) -> asserter x1 x2) |> Seq.length
    ()

let assertEqualTuple fstAsserter sndAsserter (a1, a2) (b1, b2) =
    fstAsserter a1 b1
    sndAsserter a2 b2


let assertEqualTuple3 fstAsserter sndAsserter trdAsserter (a1, a2, a3) (b1, b2, b3) =
    fstAsserter a1 b1
    sndAsserter a2 b2
    trdAsserter a3 b3

let assertEqualInt (a: int) b = Assert.Equal(a, b)

let assertEqualChar (a: char) b = Assert.Equal(a, b)

let assertEqualStr (a: string) b = Assert.Equal(a, b)

[<Fact>]
let ``Problem 77`` () =
    let actual = EightQueensProblem.queensPositions 4
    let expected = [ [ 3; 1; 4; 2 ]; [ 2; 4; 1; 3 ] ]
    assertEqualList (assertEqualList assertEqualInt) expected actual

[<Fact>]
let ``Problem 78`` () =
    let actual = KnightsTour.jump 2 (1, 1) |> List.length
    let expected = 1
    assertEqualInt expected actual

    let actual = KnightsTour.jump 3 (1, 1) |> List.length
    let expected = 8
    assertEqualInt expected actual

    let actual = KnightsTour.jump 3 (2, 2) |> List.length
    let expected = 1
    assertEqualInt expected actual

    let actual = KnightsTour.jump 8 (1, 1) |> List.length
    let expected = 64
    assertEqualInt expected actual

[<Fact>]
let ``Problem 79`` () =
    let tree =
        [ 'a'; 'b'; 'c'; 'd'; 'e'; 'f'; 'g' ], [ 'd', 'a'; 'a', 'g'; 'a', 'b'; 'b', 'c'; 'b', 'e'; 'e', 'f' ]

    let actualNodes, actualEdges = VonKochsConjecture.label tree
    let expectedNodes = [ 'a', 1; 'b', 5; 'c', 2; 'd', 6; 'e', 3; 'f', 4; 'g', 7 ]

    let expectedEdges =
        [ 'd', 'a', 5; 'a', 'g', 6; 'a', 'b', 4; 'b', 'c', 3; 'b', 'e', 2; 'e', 'f', 1 ]

    assertEqualList (assertEqualTuple assertEqualChar assertEqualInt) expectedNodes actualNodes
    assertEqualList (assertEqualTuple3 assertEqualChar assertEqualChar assertEqualInt) expectedEdges actualEdges

[<Fact>]
let ``Problem 80`` () =
    let actual = AritmeticPuzzle.solve [ 2; 3; 5; 7; 11 ]
    let expected = "2 = (((3) - 5) - 7) + 11"
    assertEqualStr expected actual

[<Fact>]
let ``Problem 81`` () =
    let actual = EnglishNumberWords.fullWords 175
    let expected = "one-seven-five"
    assertEqualStr expected actual

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
    assertEqualArray (assertEqualArray assertEqualInt) expected actual

[<Fact>]
let ``Problem 84`` () =
    let rows = [ [ 3 ]; [ 1 ]; [ 1; 1 ]; [ 1 ] ]
    let cols = [ [ 1 ]; [ 1; 1 ]; [ 1; 1 ]; [ 2 ] ]

    let actual = Nanograms.solve rows cols |> Option.map Nanograms.render
    let expected = " xxx\n" + "   x\n" + "x x \n" + " x  "

    Assert.True actual.IsSome
    assertEqualStr expected (Option.get actual)

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
    | Ok(actual) -> assertEqualStr expected actual

[<Fact>]
let ``Problem 86`` () =
    let stream = NeverEndingSequence.ints 5

    let actual = NeverEndingSequence.hd stream
    let expected = 5
    assertEqualInt expected actual

    let actual = NeverEndingSequence.hd (NeverEndingSequence.tl stream)
    let expected = 6
    assertEqualInt expected actual

    let actual = NeverEndingSequence.take 5 stream
    let expected = [ 5; 6; 7; 8; 9 ]
    assertEqualList assertEqualInt expected actual

    let stream = NeverEndingSequence.unfold (fun x -> x, x + 1) 0
    let actual = NeverEndingSequence.take 5 stream
    let expected = [ 0; 1; 2; 3; 4 ]
    assertEqualList assertEqualInt expected actual

    let stream = NeverEndingSequence.bang 420
    let actual = NeverEndingSequence.take 3 stream
    let expected = [ 420; 420; 420 ]
    assertEqualList assertEqualInt expected actual

    let stream = NeverEndingSequence.ints 5
    let stream = NeverEndingSequence.map (fun x -> x * 2) stream
    let actual = NeverEndingSequence.take 3 stream
    let expected = [ 10; 12; 14 ]
    assertEqualList assertEqualInt expected actual

    let stream = NeverEndingSequence.ints 5
    let stream = NeverEndingSequence.filter (fun x -> x % 2 = 0) stream
    let actual = NeverEndingSequence.take 3 stream
    let expected = [ 6; 8; 10 ]
    assertEqualList assertEqualInt expected actual

[<Fact>]
let ``Problem 87`` () =
    let actual = DiagnonalOfSequenceOfSequences.diag [ [ 1 ] ] |> Seq.toList
    let expected = [ 1 ]
    assertEqualList assertEqualInt expected actual

    let actual =
        DiagnonalOfSequenceOfSequences.diag [ [ 1; 2; 3 ]; [ 4; 5; 6 ]; [ 7; 8; 9 ] ]
        |> Seq.toList

    let expected = [ 1; 5; 9 ]
    assertEqualList assertEqualInt expected actual


    let actual =
        DiagnonalOfSequenceOfSequences.diag [ [ 1; 2; 3 ]; []; [ 7; 8; 9 ] ]
        |> Seq.toList

    let expected = [ 1; 9 ]
    assertEqualList assertEqualInt expected actual


    let actual =
        DiagnonalOfSequenceOfSequences.diag [ [ 1; 2; 3 ]; [ 4; 5; 6 ]; [ 7; 8; 9 ]; [ 10 ] ]
        |> Seq.toList

    let expected = [ 1; 5; 9 ]
    assertEqualList assertEqualInt expected actual
