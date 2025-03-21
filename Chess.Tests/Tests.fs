module Tests

open Chess
open Xunit

// TODO: Start adding more tests on checking valid moves

[<Fact>]
let ``Check knight moves`` () =
    // Top left corner
    let res = generateKnightMove (getSquare "a8")
    Assert.Equal(res.Length, 2)
    Assert.Contains(getSquare "b6", res)
    Assert.Contains(getSquare "c7", res)

    // Top right corner
    let res = generateKnightMove (getSquare "h8")
    Assert.Equal(res.Length, 2)
    Assert.Contains(getSquare "g6", res)
    Assert.Contains(getSquare "f7", res)

    // Bottom left corner
    let res = generateKnightMove (getSquare "a1")
    Assert.Equal(res.Length, 2)
    Assert.Contains(getSquare "b3", res)
    Assert.Contains(getSquare "c2", res)

    // Bottom right corner
    let res = generateKnightMove (getSquare "h1")
    Assert.Equal(res.Length, 2)
    Assert.Contains(getSquare "g3", res)
    Assert.Contains(getSquare "f2", res)

    // Random pos
    let res = generateKnightMove (getSquare "a5")
    Assert.Equal(res.Length, 4)
    Assert.Contains(getSquare "b7", res)
    Assert.Contains(getSquare "c6", res)
    Assert.Contains(getSquare "c4", res)
    Assert.Contains(getSquare "b3", res)

    let res = generateKnightMove (getSquare "c6")
    Assert.Equal(res.Length, 8)
    Assert.Contains(getSquare "b8", res)
    Assert.Contains(getSquare "d8", res)
    Assert.Contains(getSquare "a7", res)
    Assert.Contains(getSquare "e7", res)
    Assert.Contains(getSquare "a5", res)
    Assert.Contains(getSquare "e5", res)
    Assert.Contains(getSquare "b4", res)
    Assert.Contains(getSquare "d4", res)


[<Fact>]
let ``Check rook move`` () =
    // Top left corner
    let res = generateRookMove (getSquare "a8")
    Assert.Equal(res.Length, 14)
    Assert.Contains(getSquare "a7", res)
    Assert.Contains(getSquare "a6", res)
    Assert.Contains(getSquare "a5", res)
    Assert.Contains(getSquare "a4", res)
    Assert.Contains(getSquare "a3", res)
    Assert.Contains(getSquare "a2", res)
    Assert.Contains(getSquare "a1", res)
    Assert.Contains(getSquare "b8", res)
    Assert.Contains(getSquare "c8", res)
    Assert.Contains(getSquare "d8", res)
    Assert.Contains(getSquare "e8", res)
    Assert.Contains(getSquare "f8", res)
    Assert.Contains(getSquare "g8", res)
    Assert.Contains(getSquare "h8", res)

    // Center
    let res = generateRookMove (getSquare "d5")
    Assert.Equal(res.Length, 14)
    Assert.Contains(getSquare "d1", res)
    Assert.Contains(getSquare "d2", res)
    Assert.Contains(getSquare "d3", res)
    Assert.Contains(getSquare "d4", res)
    Assert.DoesNotContain(getSquare "d5", res)
    Assert.Contains(getSquare "d6", res)
    Assert.Contains(getSquare "d7", res)
    Assert.Contains(getSquare "d8", res)
    Assert.Contains(getSquare "a5", res)
    Assert.Contains(getSquare "b5", res)
    Assert.Contains(getSquare "c5", res)
    Assert.Contains(getSquare "e5", res)
    Assert.Contains(getSquare "f5", res)
    Assert.Contains(getSquare "g5", res)
    Assert.Contains(getSquare "h5", res)


[<Fact>]
let ``Check pawn move`` () =
    // White pawns
    let res = generatePawnMove (getSquare "a2") Up
    Assert.Equal(2, res.Length)
    Assert.Contains(getSquare "a3", res)
    Assert.Contains(getSquare "a4", res)

    // Black pawns
    let res = generatePawnMove (getSquare "a7") Down
    Assert.Equal(2, res.Length)
    Assert.Contains(getSquare "a6", res)
    Assert.Contains(getSquare "a5", res)

    let res = generatePawnMove (getSquare "a7") Up
    Assert.Equal(1, res.Length)

    // Invalid
    let res = generatePawnMove (getSquare "a8") Up
    Assert.Equal(0, res.Length)

    let res = generatePawnMove (getSquare "a1") Down
    Assert.Equal(0, res.Length)
