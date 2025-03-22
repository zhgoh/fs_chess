module Chess

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

type PawnDirection =
    | Up
    | Down

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
        printf $"%d{8 - i}| " // Print the left most column for the row number
        row |> List.iter (fun piece -> printf $"%c{pieceToChar piece} ") // Print the whole row
        printfn "") // Followed by newline

    printfn "  ─────────────────"
    printfn "   a b c d e f g h"

let getSquare (move: string) =
    let col = int (move[0] - 'a')
    let row = 7 - int (move[1] - '1') // Flip the board upside down for array indexing
    { col = col; row = row }

let isValidPosition loc =
    loc.row >= 0 && loc.row < 8 && loc.col >= 0 && loc.col < 8

let generatePawnMove (loc: Location) (dir: PawnDirection) =
    let offset =
        match dir with
        | Up -> -1
        | Down -> 1

    let start =
        match loc with
        | { row = 1 }
        | { row = 6 } ->
            [ { col = loc.col
                row = loc.row + offset + offset } ]
        | _ -> []

    [ { col = loc.col
        row = loc.row + offset } ]
    @ start
    |> List.filter isValidPosition

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
    match piece with
    | Piece.BlackPawn -> generatePawnMove loc Down
    | Piece.WhitePawn -> generatePawnMove loc Up
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

// Check if moves are within the board
let isMoveInBoard (move1: string) (move2: string) =

    // NOTE: It does not consider chess pieces yet
    let isValidSquare (square: string) =
        square.Length = 2
        && square[0] >= 'a'
        && square[0] <= 'h'
        && square[1] >= '1'
        && square[1] <= '8'

    move1 <> move2 && isValidSquare move1 && isValidSquare move2

// Check if the move is valid
let checkMove (move1: string) (move2: string) (board: Board) =
    match isMoveInBoard move1 move2 with
    | true ->
        let fromLoc = getSquare move1
        let toLoc = getSquare move2

        let piece = board[fromLoc.row][fromLoc.col]
        let moves = generateMoves piece fromLoc |> List.filter (fun loc -> loc = toLoc)

        match moves.Length with
        | 0 ->
            printfn "No valid moves."
            false
        | _ ->
            printfn $"Moving %A{piece} from %s{move1} to %s{move2}"
            true
    | false -> false

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
        | 2 -> ChessMove(parts[0], parts[1])
        | _ -> Invalid
    | _ -> Invalid

let updateCell loc piece board =
    let setVal r row c col cell =
        match r = row && c = col with
        | true -> piece
        | false -> cell

    board
    |> List.mapi (fun r rowList -> rowList |> List.mapi (fun c -> setVal r loc.row c loc.col))

let movePiece (move1: string) (move2: string) (board: Board) =
    let move1 = getSquare move1
    let move2 = getSquare move2
    let piece = board[move1.row][move1.col]
    board |> updateCell move1 Piece.Blank |> updateCell move2 piece

// Main game loop
let rec gameLoop (board: Board) =
    printBoard board
    printf "\nEnter move (e.g. 'e2 e4') or 'quit' to exit: "

    let rec restart board =
        Console.ReadLine() |> ignore
        board |> gameLoop

    let input = Console.ReadLine()
    let command = parseInput input

    // Validate moves
    let command =
        match command with
        | ChessMove(move1, move2) ->
            match board |> checkMove move1 move2 with
            | true -> ChessMove(move1, move2)
            | false -> Invalid
        | _ -> command

    // Proceed with moves and update board
    match command with
    | Quit -> printfn "Thank you for playing. Press enter to continue."
    | Invalid ->
        printfn "Invalid input, please try again."
        board |> restart
    | Castle castle -> board |> restart
    | ChessMove(move1, move2) ->
        let board = board |> movePiece move1 move2
        board |> restart


// Start the game
let board = initBoard
board |> gameLoop
