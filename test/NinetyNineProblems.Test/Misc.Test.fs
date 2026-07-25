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

let assertEqualTuple fstAsserter sndAsserter (a1, a2) (b1, b2) =
    fstAsserter a1 b1
    sndAsserter a2 b2


let assertEqualTuple3 fstAsserter sndAsserter trdAsserter (a1, a2, a3) (b1, b2, b3) =
    fstAsserter a1 b1
    sndAsserter a2 b2
    trdAsserter a3 b3

let assertEqualInt (a: int) b = Assert.Equal(a, b)

let assertEqualChar (a: char) b = Assert.Equal(a, b)

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
