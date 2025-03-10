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
    let black_pawns = List.init 8 (fun _ -> BLACK_PAWN)
    let white_pawns = List.init 8 (fun _ -> WHITE_PAWN)

    let board =
        [ [ BLACK_ROOK
            BLACK_KNIGHT
            BLACK_BISHOP
            BLACK_QUEEN
            BLACK_KING
            BLACK_BISHOP
            BLACK_KNIGHT
            BLACK_ROOK ]
          black_pawns
          row
          row
          row
          row
          white_pawns
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

// Define a type to represent the possible commands
type Command =
    | Quit
    | ChessMove of string * string
    | Castle of string
    | Invalid

// Check if moves are valid
let validateMove move1 move2 =

    // NOTE: It does not consider chess pieces yet
    let isValidSquare (square: string) =
        square.Length = 2
        && square.[0] >= 'a'
        && square.[0] <= 'h'
        && square.[1] >= '1'
        && square.[1] <= '8'

    move1 <> move2 && isValidSquare move1 && isValidSquare move2

// Function to parse the input string into a Command
let parseInput (input: string) =
    // Make sure input is valid
    let trimmedInput = input.Trim().ToLower()

    match trimmedInput with
    | "q"
    | "quit" -> Quit
    | "0-0-0"
    | "0-0" -> Castle trimmedInput
    | input when input.Contains " " ->
        let parts = input.Split ' '

        match parts.Length with
        | 2 -> ChessMove(parts.[0], parts.[1])
        | _ -> Invalid
    | _ -> Invalid

let checkMove move1 move2 (board: list<list<char>>) =
    let getSquare (move: string) =
        let col = int move.[0] - int 'a'
        let row = int move.[1] - int '1'
        row, col

    let fromRow, fromCol = getSquare move1
    let toRow, toCol = getSquare move2

    // TODO: Flip the row since we count from bottom for chess but list starts from top
    printfn "%d %d %d %d" fromRow fromCol toRow toCol
    board.[fromRow].[fromCol] <> ' ' && board.[toRow].[toCol] = ' '

let processInput input board =
    let parsed = parseInput input

    match parsed with
    | Quit -> printfn "Thank you for playing."
    | ChessMove(move1, move2) ->
        match validateMove move1 move2 with
        | true ->
            // TODO: Check board state here
            match checkMove move1 move2 board with
            | true -> printfn "move: %s %s" move1 move2
            | false -> printfn "Invalid input, please try again."
        | false -> printfn "Invalid input, please try again."
    | Castle castle -> printfn "castle: %s" castle
    | Invalid -> printfn "Invalid input, please try again."

    Console.ReadLine() |> ignore

    parsed


// Main game loop
let rec gameLoop board =
    printBoard board
    printf "\nEnter move (e.g. 'e2 e4') or 'quit' to exit: "

    let input = Console.ReadLine()

    match processInput input board with
    | Quit -> ()
    | _ -> gameLoop board

// Start the game
let board = initBoard
gameLoop board
