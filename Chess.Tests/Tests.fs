module Tests

open Chess
open Xunit

let hasNoDuplicates list =
    let distinctList = List.distinct list
    list.Length = distinctList.Length

let assertValid (expected: list<string>) (res: list<Location>) =
    Assert.Equal(expected.Length, res.Length)
    Assert.True(hasNoDuplicates expected)
    Assert.True(hasNoDuplicates res)

    let isValid (res: list<Location>) =
        let pred (loc: string) = Assert.Contains(getSquare loc, res)
        pred

    expected |> List.iter (isValid res)

/// Places a piece (represented as a ChessPiece) as the selected location
let placePiece loc piece (board: ChessPiece[,]) =
    // Create a copy of the board to maintain immutability
    let newBoard = Array2D.copy board
    newBoard[loc.row, loc.col] <- piece
    newBoard


[<Fact>]
let ``Check pawn move`` () =
    // Create empty board
    let emptyBoard = Array2D.create 8 8 ChessPiece.Blank

    // White pawns first row
    let expected = [ "a3"; "a4" ]
    let board = emptyBoard |> placePiece (getSquare "a2") ChessPiece.WhitePawn
    let res = generatePawnMove board (getSquare "a2") ChessPiece.WhitePawn Up
    assertValid expected res

    // Black pawns first row
    let expected = [ "a5"; "a6" ]
    let board = emptyBoard |> placePiece (getSquare "a7") ChessPiece.BlackPawn
    let res = generatePawnMove board (getSquare "a7") ChessPiece.BlackPawn Down
    assertValid expected res

    // Normal move up
    let expected = [ "a4" ]
    let board = emptyBoard |> placePiece (getSquare "a3") ChessPiece.WhitePawn
    let res = generatePawnMove board (getSquare "a3") ChessPiece.WhitePawn Up
    assertValid expected res

    // Normal move down
    let expected = [ "a1" ]
    let board = emptyBoard |> placePiece (getSquare "a3") ChessPiece.BlackPawn
    let res = generatePawnMove board (getSquare "a2") ChessPiece.BlackPawn Down
    assertValid expected res

    // Reached border
    let expected = []
    let board = emptyBoard |> placePiece (getSquare "a8") ChessPiece.WhitePawn
    let res = generatePawnMove board (getSquare "a8") ChessPiece.WhitePawn Up
    assertValid expected res

    let expected = []
    let board = emptyBoard |> placePiece (getSquare "a1") ChessPiece.BlackPawn
    let res = generatePawnMove board (getSquare "a1") ChessPiece.BlackPawn Down
    assertValid expected res

    // White pawns first row with enemy blocking
    let expected = []

    let board =
        emptyBoard
        |> placePiece (getSquare "a2") ChessPiece.WhitePawn
        |> placePiece (getSquare "a3") ChessPiece.BlackPawn

    let res = generatePawnMove board (getSquare "a2") ChessPiece.WhitePawn Up
    assertValid expected res

[<Fact>]
let ``Check pawn capture`` () =
    // Create empty board
    let emptyBoard = Array2D.create 8 8 ChessPiece.Blank

    // White pawns first row with no enemy blocking
    let expected = [ "a3"; "a4"; "b3" ]

    let board =
        emptyBoard
        |> placePiece (getSquare "a2") ChessPiece.WhitePawn
        |> placePiece (getSquare "b3") ChessPiece.BlackPawn

    let res = generatePawnMove board (getSquare "a2") ChessPiece.WhitePawn Up
    assertValid expected res

    // White pawns first row with enemy blocking
    let expected = [ "b3" ]

    let board =
        emptyBoard
        |> placePiece (getSquare "a2") ChessPiece.WhitePawn
        |> placePiece (getSquare "b3") ChessPiece.BlackPawn
        |> placePiece (getSquare "a3") ChessPiece.BlackPawn

    let res = generatePawnMove board (getSquare "a2") ChessPiece.WhitePawn Up
    assertValid expected res


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

    // Top right corner
    let expected =
        [ "h7"
          "h6"
          "h5"
          "h4"
          "h3"
          "h2"
          "h1"
          "a8"
          "b8"
          "c8"
          "d8"
          "e8"
          "f8"
          "g8" ]

    let res = generateRookMove (getSquare "h8")
    assertValid expected res

    // Bottom left corner
    let expected =
        [ "a8"
          "a7"
          "a6"
          "a5"
          "a4"
          "a3"
          "a2"
          "b1"
          "c1"
          "d1"
          "e1"
          "f1"
          "g1"
          "h1" ]

    let res = generateRookMove (getSquare "a1")
    assertValid expected res

    // Bottom right corner
    let expected =
        [ "h8"
          "h7"
          "h6"
          "h5"
          "h4"
          "h3"
          "h2"
          "a1"
          "b1"
          "c1"
          "d1"
          "e1"
          "f1"
          "g1" ]

    let res = generateRookMove (getSquare "h1")
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

[<Fact>]
let ``Check queen move`` () =
    // Top left
    let expected =
        [ "b7"
          "c6"
          "d5"
          "e4"
          "f3"
          "g2"
          "h1"
          "a7"
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

    let res = generateQueenMove (getSquare "a8")
    assertValid expected res

    // Top right
    let expected =
        [ "h7"
          "h6"
          "h5"
          "h4"
          "h3"
          "h2"
          "h1"
          "a8"
          "b8"
          "c8"
          "d8"
          "e8"
          "f8"
          "g8"
          "g7"
          "f6"
          "e5"
          "d4"
          "c3"
          "b2"
          "a1" ]

    let res = generateQueenMove (getSquare "h8")
    assertValid expected res

    // Bottom left corner
    let expected =
        [ "a8"
          "a7"
          "a6"
          "a5"
          "a4"
          "a3"
          "a2"
          "b1"
          "c1"
          "d1"
          "e1"
          "f1"
          "g1"
          "h1"
          "b2"
          "c3"
          "d4"
          "e5"
          "f6"
          "g7"
          "h8" ]

    let res = generateQueenMove (getSquare "a1")
    assertValid expected res

    // Bottom right corner
    let expected =
        [ "h8"
          "h7"
          "h6"
          "h5"
          "h4"
          "h3"
          "h2"
          "a1"
          "b1"
          "c1"
          "d1"
          "e1"
          "f1"
          "g1"
          "a8"
          "b7"
          "c6"
          "d5"
          "e4"
          "f3"
          "g2" ]

    let res = generateQueenMove (getSquare "h1")
    assertValid expected res

[<Fact>]
let ``Check king move`` () =
    // Top left
    let expected = [ "a7"; "b7"; "b8" ]
    let res = generateKingMove (getSquare "a8")
    assertValid expected res

    // Top right
    let expected = [ "h7"; "g7"; "g8" ]
    let res = generateKingMove (getSquare "h8")
    assertValid expected res

    // Bottom left
    let expected = [ "a2"; "b2"; "b1" ]
    let res = generateKingMove (getSquare "a1")
    assertValid expected res

    // Bottom right
    let expected = [ "g1"; "g2"; "h2" ]
    let res = generateKingMove (getSquare "h1")
    assertValid expected res

    // Center
    let expected = [ "c5"; "c6"; "d6"; "e6"; "e5"; "e4"; "d4"; "c4" ]
    let res = generateKingMove (getSquare "d5")
    assertValid expected res

[<Fact>]
let ``Check if pawn promotion works`` () =
    // Black promote
    let board =
        Array2D.create 8 8 ChessPiece.Blank
        |> placePiece (getSquare "a2") ChessPiece.BlackPawn

    let loc = getSquare "a2"
    let piece = board[loc.row, loc.col]
    let dst = getSquare "a1"

    Assert.True(hasPromotion piece dst)

    // White promote
    let board =
        Array2D.create 8 8 ChessPiece.Blank
        |> placePiece (getSquare "a7") ChessPiece.WhitePawn

    let loc = getSquare "a7"
    let piece = board[loc.row, loc.col]
    let dst = getSquare "a8"

    Assert.True(hasPromotion piece dst)
