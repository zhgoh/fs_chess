let BLACK_PAWN = '♟'
let WHITE_PAWN = '♙'
let BLACK_ROOK = '♜'
let WHITE_ROOK = '♖'
let BLACK_KNIGHT = '♞'
let WHITE_KNIGHT = '♘'
let BLACK_BISHOP = '♝'
let WHITE_BISHOP = '♗'
let BLACK_QUEEN = '♛'
let WHITE_QUEEN = '♕'
let BLACK_KING = '♚'
let WHITE_KING = '♔'

let initBoard =
    let row = List.init 8 (fun _ -> '.')

    let board =
        [ [ BLACK_ROOK
            BLACK_KNIGHT
            BLACK_BISHOP
            BLACK_QUEEN
            BLACK_KING
            BLACK_BISHOP
            BLACK_KNIGHT
            BLACK_ROOK ]
          [ BLACK_PAWN
            BLACK_PAWN
            BLACK_PAWN
            BLACK_PAWN
            BLACK_PAWN
            BLACK_PAWN
            BLACK_PAWN
            BLACK_PAWN ]
          row
          row
          row
          row
          [ WHITE_PAWN
            WHITE_PAWN
            WHITE_PAWN
            WHITE_PAWN
            WHITE_PAWN
            WHITE_PAWN
            WHITE_PAWN
            WHITE_PAWN ]
          [ WHITE_ROOK
            WHITE_KNIGHT
            WHITE_BISHOP
            WHITE_QUEEN
            WHITE_KING
            WHITE_BISHOP
            WHITE_KNIGHT
            WHITE_ROOK ] ]

    board

let printBoard board =
    printfn "   a b c d e f g h"
    printfn "  ─────────────────"

    board
    |> List.iteri (fun i row ->
        printf "%d| " (8 - i)
        row |> List.iter (fun piece -> printf "%c " piece)
        printfn "")

    printfn "  ─────────────────"
    printfn "   a b c d e f g h"

let board = initBoard
printBoard board
