namespace NinetyNineProblems.Test

module BinaryTreesTests =

    open NinetyNineProblems.BinaryTrees
    open Xunit
    open Asserter

    let leaf x = Node(x, Empty, Empty)

    let rec binaryTreeEqual equal expected actual =
        match expected, actual with
        | Empty, Empty -> ()
        | Node(e, el, er), Node(a, al, ar) ->
            equal e a
            binaryTreeEqual equal el al
            binaryTreeEqual equal er ar
        | _ -> fail "Different shapes"

    [<Fact>]
    let ``Problem 44`` () =
        let actual = cbalTree 4

        let expected =
            [ Node('x', Node('x', Empty, Empty), Node('x', Node('x', Empty, Empty), Empty))
              Node('x', Node('x', Empty, Empty), Node('x', Empty, Node('x', Empty, Empty)))
              Node('x', Node('x', Node('x', Empty, Empty), Empty), Node('x', Empty, Empty))
              Node('x', Node('x', Empty, Node('x', Empty, Empty)), Node('x', Empty, Empty)) ]

        listEqual (binaryTreeEqual charEqual) expected actual

    [<Fact>]
    let ``Problem 45`` () =
        isTrue (isSymmetric Empty)
        isTrue (isSymmetric (Node(1, Empty, Empty)))
        isTrue (isSymmetric (Node(1, Node(2, Empty, Empty), Node(3, Empty, Empty))))
        isFalse (isSymmetric (Node(1, Node(2, Empty, Node(69, Empty, Empty)), Node(3, Empty, Empty))))

    [<Fact>]
    let ``Problem 46`` () =
        let actual = construct [ 3; 2; 5; 7; 1 ]

        let expected =
            Node(3, Node(2, Node(1, Empty, Empty), Empty), Node(5, Empty, Node(7, Empty, Empty)))

        binaryTreeEqual intEqual expected actual

        isTrue (isSymmetric (construct [ 5; 3; 18; 1; 4; 12; 21 ]))
        isFalse (isSymmetric (construct [ 3; 2; 5; 7; 4 ]))

    [<Fact>]
    let ``Problem 47`` () =
        let actual = symCbalTrees 5

        let expected =
            [ Node('x', Node('x', Node('x', Empty, Empty), Empty), Node('x', Empty, Node('x', Empty, Empty)))
              Node('x', Node('x', Empty, Node('x', Empty, Empty)), Node('x', Node('x', Empty, Empty), Empty)) ]

        listEqual (binaryTreeEqual charEqual) expected actual

        let actual = List.length (symCbalTrees 57)
        let expected = 256
        intEqual expected actual

    [<Fact>]
    let ``Problem 48`` () =
        let actual = hbalTree 3

        let expected =
            [ Node('x', Node('x', Empty, Node('x', Empty, Empty)), Node('x', Empty, Node('x', Empty, Empty)))
              Node('x', Node('x', Empty, Node('x', Empty, Empty)), Node('x', Node('x', Empty, Empty), Empty))
              Node(
                  'x',
                  Node('x', Empty, Node('x', Empty, Empty)),
                  Node('x', Node('x', Empty, Empty), Node('x', Empty, Empty))
              )
              Node('x', Node('x', Node('x', Empty, Empty), Empty), Node('x', Empty, Node('x', Empty, Empty)))
              Node('x', Node('x', Node('x', Empty, Empty), Empty), Node('x', Node('x', Empty, Empty), Empty))
              Node(
                  'x',
                  Node('x', Node('x', Empty, Empty), Empty),
                  Node('x', Node('x', Empty, Empty), Node('x', Empty, Empty))
              )
              Node(
                  'x',
                  Node('x', Node('x', Empty, Empty), Node('x', Empty, Empty)),
                  Node('x', Empty, Node('x', Empty, Empty))
              )
              Node(
                  'x',
                  Node('x', Node('x', Empty, Empty), Node('x', Empty, Empty)),
                  Node('x', Node('x', Empty, Empty), Empty)
              )
              Node(
                  'x',
                  Node('x', Node('x', Empty, Empty), Node('x', Empty, Empty)),
                  Node('x', Node('x', Empty, Empty), Node('x', Empty, Empty))
              )
              Node('x', Node('x', Empty, Node('x', Empty, Empty)), Node('x', Empty, Empty))
              Node('x', Node('x', Node('x', Empty, Empty), Empty), Node('x', Empty, Empty))
              Node('x', Node('x', Node('x', Empty, Empty), Node('x', Empty, Empty)), Node('x', Empty, Empty))
              Node('x', Node('x', Empty, Empty), Node('x', Empty, Node('x', Empty, Empty)))
              Node('x', Node('x', Empty, Empty), Node('x', Node('x', Empty, Empty), Empty))
              Node('x', Node('x', Empty, Empty), Node('x', Node('x', Empty, Empty), Node('x', Empty, Empty))) ]

        listEqual (binaryTreeEqual charEqual) expected actual

    [<Fact>]
    let ``Problem 49`` () =
        let actual = List.length (hbalTreeNodes 15)
        let expected = 1553
        intEqual expected actual

    [<Fact>]
    let ``Problem 50`` () =
        let actual = countLeaves Empty
        let expected = 0
        intEqual expected actual

    [<Fact>]
    let ``Problem 51`` () =
        let actual = leaves Empty
        let expected = []
        listEqual intEqual expected actual

    [<Fact>]
    let ``Problem 52`` () =
        let actual = internals (Node('a', Empty, Empty))
        let expected = []
        listEqual charEqual expected actual

    [<Fact>]
    let ``Problem 53`` () =
        let exampleTree =
            Node(
                'a',
                Node('b', Node('d', Empty, Empty), Node('e', Empty, Empty)),
                Node('c', Empty, Node('f', Node('g', Empty, Empty), Empty))
            )

        let actual = atLevel exampleTree 2

        let expected = [ 'b'; 'c' ]
        listEqual charEqual expected actual

    [<Fact>]
    let ``Problem 54`` () =
        let actual = completeBinaryTree [ 1; 2; 3; 4; 5; 6 ]

        let expected =
            Node(1, Node(2, Node(4, Empty, Empty), Node(5, Empty, Empty)), Node(3, Node(6, Empty, Empty), Empty))

        binaryTreeEqual intEqual expected actual

    [<Fact>]
    let ``Problem 55`` () =
        let exampleLayoutTree =
            Node(
                'n',
                Node('k', Node('c', leaf 'a', Node('h', Node('g', leaf 'e', Empty), Empty)), leaf 'm'),
                Node('u', Node('p', Empty, Node('s', leaf 'q', Empty)), Empty)
            )

        let actual = layoutBinaryTree1 exampleLayoutTree

        let expected =
            Node(
                ('n', 8, 1),
                Node(
                    ('k', 6, 2),
                    Node(
                        ('c', 2, 3),
                        Node(('a', 1, 4), Empty, Empty),
                        Node(('h', 5, 4), Node(('g', 4, 5), Node(('e', 3, 6), Empty, Empty), Empty), Empty)
                    ),
                    Node(('m', 7, 3), Empty, Empty)
                ),
                Node(
                    ('u', 12, 2),
                    Node(('p', 9, 3), Empty, Node(('s', 11, 4), Node(('q', 10, 5), Empty, Empty), Empty)),
                    Empty
                )
            )

        binaryTreeEqual (tupleEqual3 charEqual intEqual intEqual) expected actual

    [<Fact>]
    let ``Problem 56`` () =
        let exampleLayoutTree =
            Node(
                'n',
                Node('k', Node('c', leaf 'a', Node('e', leaf 'd', leaf 'g')), leaf 'm'),
                Node('u', Node('p', Empty, leaf 'q'), Empty)
            )

        let actual = layoutBinaryTree2 exampleLayoutTree

        let expected =
            Node(
                ('n', 15, 1),
                Node(
                    ('k', 7, 2),
                    Node(
                        ('c', 3, 3),
                        Node(('a', 1, 4), Empty, Empty),
                        Node(('e', 5, 4), Node(('d', 4, 5), Empty, Empty), Node(('g', 6, 5), Empty, Empty))
                    ),
                    Node(('m', 11, 3), Empty, Empty)
                ),
                Node(('u', 23, 2), Node(('p', 19, 3), Empty, Node(('q', 21, 4), Empty, Empty)), Empty)
            )

        binaryTreeEqual (tupleEqual3 charEqual intEqual intEqual) expected actual

    [<Fact>]
    let ``Problem 57`` () =
        let exampleLayoutTree =
            Node(
                'n',
                Node('k', Node('c', leaf 'a', Node('h', Node('g', leaf 'e', Empty), Empty)), leaf 'm'),
                Node('u', Node('p', Empty, Node('s', leaf 'q', Empty)), Empty)
            )

        let actual = layoutBinaryTree3 exampleLayoutTree

        let expected =
            Node(
                ('n', 5, 1),
                Node(
                    ('k', 3, 2),
                    Node(
                        ('c', 2, 3),
                        Node(('a', 1, 4), Empty, Empty),
                        Node(('h', 3, 4), Node(('g', 2, 5), Node(('e', 1, 6), Empty, Empty), Empty), Empty)
                    ),
                    Node(('m', 4, 3), Empty, Empty)
                ),
                Node(
                    ('u', 7, 2),
                    Node(('p', 6, 3), Empty, Node(('s', 7, 4), Node(('q', 6, 5), Empty, Empty), Empty)),
                    Empty
                )
            )

        binaryTreeEqual (tupleEqual3 charEqual intEqual intEqual) expected actual

    [<Fact>]
    let ``Problem 58`` () =
        let str = "a(b(d,e),c(,f(g,)))"

        let tree =
            Node('a', Node('b', leaf 'd', leaf 'e'), Node('c', Empty, Node('f', leaf 'g', Empty)))

        let actual = stringOfTree tree
        let expected = str
        strEqual expected actual

        let actual = treeOfString str
        let expected = tree

        binaryTreeEqual charEqual expected actual

    [<Fact>]
    let ``Problem 59`` () =
        let expected =
            Node(
                'n',
                Node('k', Node('c', leaf 'a', Node('h', Node('g', leaf 'e', Empty), Empty)), leaf 'm'),
                Node('u', Node('p', Empty, Node('s', leaf 'q', Empty)), Empty)
            )

        let preordered = preorder expected
        let inordered = inorder expected
        let actual = preInTree preordered inordered

        binaryTreeEqual charEqual expected actual

    [<Fact>]
    let ``Problem 60`` () =
        let str = "abd..e..c.fg..."

        let tree =
            Node('a', Node('b', leaf 'd', leaf 'e'), Node('c', Empty, Node('f', leaf 'g', Empty)))

        let actual = dotstringOfTree tree
        let expected = str
        strEqual expected actual

        let actual = treeOfDotstring str
        let expected = tree
        binaryTreeEqual charEqual expected actual
