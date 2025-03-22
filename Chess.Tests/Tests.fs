module Tests

open Chess
open Xunit

// TODO: Start adding more tests on checking valid moves


let assertValid (expected: list<string>) (res: list<Location>) =
    Assert.Equal(expected.Length, res.Length)

    let isValid (res: list<Location>) =
        let pred (loc: string) = Assert.Contains(getSquare loc, res)
        pred

    expected |> List.iter (isValid res)


[<Fact>]
let ``Check knight moves`` () =
    // Top left corner
    let expected = [ "b6"; "c7" ]
    let res = generateKnightMove (getSquare "a8")
    assertValid expected res

    // Top right corner
    let expected = [ "g6"; "f7" ]
    let res = generateKnightMove (getSquare "h8")
    assertValid expected res

    // Bottom left corner
    let expected = [ "b3"; "c2" ]
    let res = generateKnightMove (getSquare "a1")
    assertValid expected res

    // Bottom right corner
    let expected = [ "g3"; "f2" ]
    let res = generateKnightMove (getSquare "h1")
    assertValid expected res

    // At the side of the board
    let expected = [ "b7"; "c6"; "c4"; "b3" ]
    let res = generateKnightMove (getSquare "a5")
    assertValid expected res

    // In the middle of the board
    let expected = [ "b8"; "d8"; "a7"; "e7"; "a5"; "e5"; "b4"; "d4" ]
    let res = generateKnightMove (getSquare "c6")
    assertValid expected res


[<Fact>]
let ``Check rook move`` () =
    // Top left corner
    let expected =
        [ "a7"
          "a6"
          "a5"
          "a4"
          "a3"
          "a2"
          "a1"
          "b8"
          "c8"
          "d8"
          "e8"
          "f8"
          "g8"
          "h8" ]

    let res = generateRookMove (getSquare "a8")
    assertValid expected res

    // Center
    let expected =
        [ "d1"
          "d2"
          "d3"
          "d4"
          "d6"
          "d7"
          "d8"
          "a5"
          "b5"
          "c5"
          "e5"
          "f5"
          "g5"
          "h5" ]

    let res = generateRookMove (getSquare "d5")
    assertValid expected res


[<Fact>]
let ``Check pawn move`` () =
    // White pawns
    let expected = [ "a3"; "a4" ]
    let res = generatePawnMove (getSquare "a2") Up
    assertValid expected res

    // Black pawns
    let expected = [ "a5"; "a6" ]
    let res = generatePawnMove (getSquare "a7") Down
    assertValid expected res

    // Normal move up
    let expected = [ "a4" ]
    let res = generatePawnMove (getSquare "a3") Up
    assertValid expected res

    // Normal move down
    let expected = [ "a1" ]
    let res = generatePawnMove (getSquare "a2") Down
    assertValid expected res

    // Reached border
    let expected = []
    let res = generatePawnMove (getSquare "a8") Up
    assertValid expected res

    let expected = []
    let res = generatePawnMove (getSquare "a1") Down
    assertValid expected res

[<Fact>]
let ``Check bishop move`` () =
    // Top left
    let expected = [ "b7"; "c6"; "d5"; "e4"; "f3"; "g2"; "h1" ]
    let res = generateBishopMove (getSquare "a8")
    assertValid expected res

    // Top right
    let expected = [ "g7"; "f6"; "e5"; "d4"; "c3"; "b2"; "a1" ]
    let res = generateBishopMove (getSquare "h8")
    assertValid expected res

    // Bottom left
    let expected = [ "b2"; "c3"; "d4"; "e5"; "f6"; "g7"; "h8" ]
    let res = generateBishopMove (getSquare "a1")
    assertValid expected res

    // Bottom right
    let expected = [ "a8"; "b7"; "c6"; "d5"; "e4"; "f3"; "g2" ]
    let res = generateBishopMove (getSquare "h1")
    assertValid expected res
