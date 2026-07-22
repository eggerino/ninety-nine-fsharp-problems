module BinaryTreesTests

open NinetyNineProblems.BinaryTrees
open Xunit
open ListTests

[<Fact>]
let ``Problem 44`` () =
    let actual = cbalTree 4

    let expected =
        [ Node('x', Node('x', Empty, Empty), Node('x', Node('x', Empty, Empty), Empty))
          Node('x', Node('x', Empty, Empty), Node('x', Empty, Node('x', Empty, Empty)))
          Node('x', Node('x', Node('x', Empty, Empty), Empty), Node('x', Empty, Empty))
          Node('x', Node('x', Empty, Node('x', Empty, Empty)), Node('x', Empty, Empty)) ]

    assertEqualStr expected actual

[<Fact>]
let ``Problem 45`` () =
    Assert.True(isSymmetric Empty)
    Assert.True(isSymmetric (Node(1, Empty, Empty)))
    Assert.True(isSymmetric (Node(1, Node(2, Empty, Empty), Node(3, Empty, Empty))))
    Assert.False(isSymmetric (Node(1, Node(2, Empty, Node(69, Empty, Empty)), Node(3, Empty, Empty))))

[<Fact>]
let ``Problem 46`` () =
    let actual = construct [ 3; 2; 5; 7; 1 ]

    let expected =
        Node(3, Node(2, Node(1, Empty, Empty), Empty), Node(5, Empty, Node(7, Empty, Empty)))

    assertEqualStr expected actual

    Assert.True(isSymmetric (construct [ 5; 3; 18; 1; 4; 12; 21 ]))
    Assert.False(isSymmetric (construct [ 3; 2; 5; 7; 4 ]))

[<Fact>]
let ``Problem 47`` () =
    let actual = symCbalTrees 5

    let expected =
        [ Node('x', Node('x', Node('x', Empty, Empty), Empty), Node('x', Empty, Node('x', Empty, Empty)))
          Node('x', Node('x', Empty, Node('x', Empty, Empty)), Node('x', Node('x', Empty, Empty), Empty)) ]

    assertEqualStr expected actual

    let actual = List.length (symCbalTrees 57)
    let expected = 256
    Assert.Equal(expected, actual)

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

    assertEqualStr expected actual

[<Fact>]
let ``Problem 49`` () =
    let actual = List.length (hbalTreeNodes 15)
    let expected = 1553
    Assert.Equal(expected, actual)

[<Fact>]
let ``Problem 50`` () =
    let actual = countLeaves Empty
    let expected = 0
    Assert.Equal(expected, actual)

[<Fact>]
let ``Problem 51`` () =
    let actual = leaves Empty
    let expected = []
    assertEqualStr expected actual

[<Fact>]
let ``Problem 52`` () =
    let actual = internals (Node('a', Empty, Empty))
    let expected = []
    assertEqualStr expected actual

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
    assertEqualStr expected actual

[<Fact>]
let ``Problem 54`` () =
    let actual = completeBinaryTree [ 1; 2; 3; 4; 5; 6 ]

    let expected =
        Node(1, Node(2, Node(4, Empty, Empty), Node(5, Empty, Empty)), Node(3, Node(6, Empty, Empty), Empty))

    assertEqualStr expected actual

[<Fact>]
let ``Problem 55`` () =
    let exampleLayoutTree =
        let leaf x = Node(x, Empty, Empty)

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

    assertEqualStr expected actual

[<Fact>]
let ``Problem 56`` () =
    let exampleLayoutTree =
        let leaf x = Node(x, Empty, Empty)

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

    assertEqualStr expected actual
