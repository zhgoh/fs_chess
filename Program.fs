open System

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
    Console.Write "\u001bc\x1b[3J"

    printfn "   a b c d e f g h"
    printfn "  ─────────────────"

    board
    |> List.iteri (fun i row ->
        printf "%d| " (8 - i) // Print the left most column for the row number
        row |> List.iter (fun piece -> printf "%c " piece) // Print the whole row
        printfn "") // Followed by newline

    printfn "  ─────────────────"
    printfn "   a b c d e f g h"

let validateMoves input = false

// Main game loop
let rec gameLoop board =
    printBoard board
    printf "\nEnter move (e.g. 'e2 e4') or 'quit' to exit: "
    let input = Console.ReadLine()

    match input.ToLower() with
    | "quit" -> printfn "Thanks for playing!"
    | _ ->
        // TODO: implement move validation and execution
        match validateMoves input with
        | true -> printfn "Valid moves"
        | false -> printfn "Invalid moves, please try again."

        gameLoop board

// Start the game
let board = initBoard
gameLoop board
