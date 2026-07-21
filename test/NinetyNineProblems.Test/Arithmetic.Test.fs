module ArithmeticTests

open NinetyNineProblems.Arithmetic
open Xunit
open ListTests

[<Fact>]
let ``Problem 29`` () =
    Assert.False(isPrime 1)
    Assert.True(isPrime 7)
    Assert.False(isPrime 12)

[<Fact>]
let ``Problem 30`` () =
    let actual = gcd 13 27
    let expected = 1
    Assert.Equal(expected, actual)

    let actual = gcd 20536 7826
    let expected = 2
    Assert.Equal(expected, actual)

[<Fact>]
let ``Problem 31`` () =
    Assert.True(coprime 13 27)
    Assert.False(coprime 20536 7826)

[<Fact>]
let ``Problem 32`` () =
    let actual = phi 10
    let expected = 4
    Assert.Equal(expected, actual)

[<Fact>]
let ``Problem 33`` () =
    let actual = factors 315
    let expected = [ 3; 3; 5; 7 ]
    assertEqualStr expected actual

[<Fact>]
let ``Problem 34`` () =
    let actual = factors2 315
    let expected = [ (3, 2); (5, 1); (7, 1) ]
    assertEqualStr expected actual

[<Fact>]
let ``Problem 35`` () =
    let actual = phiImproved 10
    let expected = 4
    Assert.Equal(expected, actual)

    let actual = phiImproved 13
    let expected = 12
    Assert.Equal(expected, actual)

[<Fact>]
let ``Problem 36`` () =
    // Isn't really a problem
    ()

[<Fact>]
let ``Problem 37`` () =
    let actual = List.length (allPrimes 2 7920)
    let expected = 1000
    Assert.Equal(expected, actual)

[<Fact>]
let ``Problem 38`` () =
    let actual = goldbach 28
    let expected = (5, 23)
    assertEqualStr expected actual

[<Fact>]
let ``Problem 39`` () =
    let actual = goldbachList 9 20

    let expected =
        [ (10, (3, 7))
          (12, (5, 7))
          (14, (3, 11))
          (16, (3, 13))
          (18, (5, 13))
          (20, (3, 17)) ]

    assertEqualStr expected actual
