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

    Assert.True(isSymmetric (construct [5; 3; 18; 1; 4; 12; 21]))
    Assert.False(isSymmetric (construct [3; 2; 5; 7; 4]))
