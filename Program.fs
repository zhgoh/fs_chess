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

// Print the game board
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

let getSquare (move: string) =
    let col = int (move.[0] - 'a')
    let row = 7 - int (move.[1] - '1') // Flip the board upside down for array indexing
    { col = col; row = row }

let isValidPosition loc =
    loc.row >= 0 && loc.row < 8 && loc.col >= 0 && loc.col < 8

let generatePawnMove (loc: Location) (dir: int) =
    // TODO: Check if pawn at start location, directions becomes 2
    [ { col = loc.col; row = loc.row + dir } ] |> List.filter isValidPosition

let generateKnightMove (loc: Location) =
    [ { col = loc.col - 1; row = loc.row - 2 }
      { col = loc.col + 1; row = loc.row - 2 }
      { col = loc.col + 2; row = loc.row - 1 }
      { col = loc.col + 2; row = loc.row + 1 }
      { col = loc.col + 1; row = loc.row + 2 }
      { col = loc.col - 1; row = loc.row + 2 }
      { col = loc.col - 2; row = loc.row + 1 }
      { col = loc.col - 2; row = loc.row - 1 } ]
    |> List.filter isValidPosition

let generateKingMove (loc: Location) =
    [ { col = loc.col; row = loc.row + 1 }
      { col = loc.col; row = loc.row - 1 }
      { col = loc.col + 1; row = loc.row }
      { col = loc.col - 1; row = loc.row }
      { col = loc.col + 1; row = loc.row + 1 }
      { col = loc.col - 1; row = loc.row - 1 }
      { col = loc.col + 1; row = loc.row - 1 }
      { col = loc.col - 1; row = loc.row + 1 } ]
    |> List.filter isValidPosition

let generateBishopMove (loc: Location) =
    let directions =
        [ { col = 1; row = 1 }
          { col = 1; row = -1 }
          { col = -1; row = 1 }
          { col = -1; row = -1 } ]

    let rec generateLine (loc: Location) (dir: Location) =
        let newPos =
            { col = loc.col + dir.col
              row = loc.row + dir.row }

        match isValidPosition newPos with
        | true -> newPos :: generateLine newPos dir
        | false -> []

    directions |> List.collect (generateLine loc)

let generateRookMove (loc: Location) =
    let directions =
        [ { col = 1; row = 0 }
          { col = -1; row = 0 }
          { col = 0; row = 1 }
          { col = 0; row = -1 } ]

    let rec generateLine (loc: Location) (dir: Location) =
        let newPos =
            { col = loc.col + dir.col
              row = loc.row + dir.row }

        match isValidPosition newPos with
        | true -> newPos :: generateLine newPos dir
        | false -> []

    directions |> List.collect (generateLine loc)

let generateQueenMove (loc: Location) =
    generateBishopMove loc @ generateRookMove loc

let generateMoves (piece: Piece) (loc: Location) =
    let moves =
        match piece with
        | Piece.BlackPawn -> generatePawnMove loc 1
        | Piece.WhitePawn -> generatePawnMove loc -1
        | Piece.BlackRook
        | Piece.WhiteRook -> generateRookMove loc
        | Piece.BlackKnight
        | Piece.WhiteKnight -> generateKnightMove loc
        | Piece.BlackBishop
        | Piece.WhiteBishop -> generateBishopMove loc
        | Piece.BlackQueen
        | Piece.WhiteQueen -> generateQueenMove loc
        | Piece.BlackKing
        | Piece.WhiteKing -> generateKingMove loc
        | _ -> []

    // printfn "%A" moves
    moves


// Check if the move is valid
let checkMove (move1: string) (move2: string) (board: Board) =
    let fromLoc = getSquare move1
    let toLoc = getSquare move2

    // printfn
    // "moving %c to %c from %A %A"
    // (pieceToChar board.[fromLoc.row].[fromLoc.col])
    // (pieceToChar board.[toLoc.row].[toLoc.col])
    // fromLoc
    // toLoc

    let piece = board.[fromLoc.row].[fromLoc.col]
    let moves = generateMoves piece fromLoc |> List.filter (fun loc -> loc = toLoc)

    match moves.Length with
    | 0 ->
        printfn "No valid moves."
        false
    | _ ->
        printfn "Valid move."
        true



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
