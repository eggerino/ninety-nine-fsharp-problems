namespace NinetyNineProblems.Test

module MultiwayTreesTests =

    open NinetyNineProblems.MultiwayTrees
    open Xunit
    open Asserter

    let rec multTreeEqual equal expected actual =
        let (T(e, ets)) = expected
        let (T(a, ats)) = actual
        equal e a
        listEqual (multTreeEqual equal) ets ats

    [<Fact>]
    let ``Problem 61`` () =
        let tree =
            T('a', [ T('f', [ T('g', []) ]); T('c', []); T('b', [ T('d', []); T('e', []) ]) ])

        let str = "afg^^c^bd^e^^^"

        let actual = stringOfTree tree
        let expected = str
        strEqual expected actual

        let actual = treeOfString str
        let expected = tree
        multTreeEqual charEqual expected actual

    [<Fact>]
    let ``Problem 62`` () =
        let actual = countNodes (T('a', [ T('f', []) ]))
        let expected = 2
        intEqual expected actual

    [<Fact>]
    let ``Problem 63`` () =
        let tree =
            T('a', [ T('f', [ T('g', []) ]); T('c', []); T('b', [ T('d', []); T('e', []) ]) ])

        let actual = ipl tree
        let expected = 9
        intEqual expected actual

    [<Fact>]
    let ``Problem 64`` () =
        let tree =
            T('a', [ T('f', [ T('g', []) ]); T('c', []); T('b', [ T('d', []); T('e', []) ]) ])

        let actual = bottomUp tree
        let expected = [ 'g'; 'f'; 'c'; 'd'; 'e'; 'b'; 'a' ]
        listEqual charEqual expected actual

    [<Fact>]
    let ``Problem 65`` () =
        let tree =
            T('a', [ T('f', [ T('g', []) ]); T('c', []); T('b', [ T('d', []); T('e', []) ]) ])

        let actual = lispy (T('a', []))
        let expected = "a"
        strEqual expected actual

        let actual = lispy (T('a', [ T('b', []) ]))
        let expected = "(a b)"
        strEqual expected actual

        let actual = lispy tree
        let expected = "(a (f g) c (b d e))"
        strEqual expected actual
