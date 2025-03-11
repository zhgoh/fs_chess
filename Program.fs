open System

type Piece =
    | BlackPawn = 0x265F
    | WhitePawn = 0x2659
    | BlackRook = 0x265C
    | WhiteRook = 0x2656
    | BlackKnight = 0x265E
    | WhiteKnight = 0x2658
    | BlackBishop = 0x265D
    | WhiteBishop = 0x2657
    | BlackQueen = 0x265B
    | WhiteQueen = 0x2655
    | BlackKing = 0x265A
    | WhiteKing = 0x2654
    | Blank = 0x2E // '.' character

type Board = list<list<Piece>>
type Location = { col: int; row: int }

// Define a type to represent the possible commands
type Command =
    | Quit
    | ChessMove of string * string
    | Castle of string
    | Invalid
// Print the pieces to char
let pieceToChar (piece: Piece) = piece |> int |> char

let initBoard =
    let row = List.init 8 (fun _ -> Piece.Blank)
    let blackPawns = List.init 8 (fun _ -> Piece.BlackPawn)
    let whitePawns = List.init 8 (fun _ -> Piece.WhitePawn)

    let board =
        [ [ Piece.BlackRook
            Piece.BlackKnight
            Piece.BlackBishop
            Piece.BlackQueen
            Piece.BlackKing
            Piece.BlackBishop
            Piece.BlackKnight
            Piece.BlackRook ]
          blackPawns
          row
          row
          row
          row
          whitePawns
          [ Piece.WhiteRook
            Piece.WhiteKnight
            Piece.WhiteBishop
            Piece.WhiteQueen
            Piece.WhiteKing
            Piece.WhiteBishop
            Piece.WhiteKnight
            Piece.WhiteRook ] ]

    board

let printBoard (board: Board) =
    Console.Write "\u001bc\x1b[3J"

    printfn "   a b c d e f g h"
    printfn "  ─────────────────"

    board
    |> List.iteri (fun i row ->
        printf "%d| " (8 - i) // Print the left most column for the row number
        row |> List.iter (fun piece -> printf "%c " (pieceToChar piece)) // Print the whole row
        printfn "") // Followed by newline

    printfn "  ─────────────────"
    printfn "   a b c d e f g h"



// Check if moves are valid
let validateMove (move1: string) (move2: string) =

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


let isEmptySpace (loc: Location) (board: Board) = board.[loc.row].[loc.col] = Piece.Blank



let getSquare (move: string) =
    let col = int (move.[0] - 'a')
    let row = 7 - int (move.[1] - '1') // Flip the board upside down for array indexing
    { col = col; row = row }

let isValidMoveTo (piece: Piece) (loc: Location) (board: Board) =
    match piece with
    | Piece.BlackPawn -> true
    | Piece.WhitePawn -> true
    | Piece.BlackRook
    | Piece.WhiteRook -> true
    | Piece.BlackKnight
    | Piece.WhiteKnight -> true
    | Piece.BlackBishop
    | Piece.WhiteBishop -> true
    | Piece.BlackQueen
    | Piece.WhiteQueen -> true
    | Piece.BlackKing
    | Piece.WhiteKing -> true
    | _ -> false

let isValidMoveFrom (loc: Location) (board: Board) =
    let ch = board.[loc.row].[loc.col]

    match ch with
    | Piece.Blank -> None
    | _ -> Some ch

// Check if the move is valid
let checkMove (move1: string) (move2: string) (board: Board) =
    let fromLoc = getSquare move1
    let toLoc = getSquare move2

    // printfn "moving %c to %c" board.[fromLoc.row].[fromLoc.col] board.[toLoc.row].[toLoc.col]
    match isValidMoveFrom fromLoc board with
    | Some ch -> isValidMoveTo ch toLoc board
    | None ->
        printfn "No valid pieces chosen."
        false



let processInput (input: string) (board: Board) =
    let parsed = parseInput input

    match parsed with
    | Quit -> printfn "Thank you for playing. Press enter to continue."
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
let rec gameLoop (board: Board) =
    printBoard board
    printf "\nEnter move (e.g. 'e2 e4') or 'quit' to exit: "

    let input = Console.ReadLine()

    match processInput input board with
    | Quit -> ()
    | _ -> gameLoop board

// Start the game
let board = initBoard
gameLoop board
