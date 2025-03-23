module Chess

open System

/// Chess pieces and the unicode char
type ChessPiece =
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

/// Used to determine the direction the pawn is facing
type PawnDirection =
    | Up
    | Down

/// Used to determine the turn
type Turn =
    | White
    | Black

/// Alias for the data in the game board
type ChessBoard = list<list<ChessPiece>>

/// Game state to store board and turn info
type State = { board: ChessBoard; turn: Turn }

/// Location is just a record of positions on the board
type Location = { col: int; row: int }

/// Possible states of the game determined by inputs
type Command =
    | Quit
    | ChessMove of string * string
    | Castle of string
    | Invalid

/// Check if location is within the chessboard
let isLocationInsideBoard loc =
    loc.row >= 0 && loc.row < 8 && loc.col >= 0 && loc.col < 8

/// Generate moves for Pawn based on the direction it is facing
let generatePawnMove (loc: Location) (dir: PawnDirection) =
    //TODO: Generate en passe moves
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
    |> List.filter isLocationInsideBoard

/// Generate moves for Knight
let generateKnightMove (loc: Location) =
    [ { col = loc.col - 1; row = loc.row - 2 }
      { col = loc.col + 1; row = loc.row - 2 }
      { col = loc.col + 2; row = loc.row - 1 }
      { col = loc.col + 2; row = loc.row + 1 }
      { col = loc.col + 1; row = loc.row + 2 }
      { col = loc.col - 1; row = loc.row + 2 }
      { col = loc.col - 2; row = loc.row + 1 }
      { col = loc.col - 2; row = loc.row - 1 } ]
    |> List.filter isLocationInsideBoard

/// Generate moves for King
let generateKingMove (loc: Location) =
    [ { col = loc.col; row = loc.row + 1 }
      { col = loc.col; row = loc.row - 1 }
      { col = loc.col + 1; row = loc.row }
      { col = loc.col - 1; row = loc.row }
      { col = loc.col + 1; row = loc.row + 1 }
      { col = loc.col - 1; row = loc.row - 1 }
      { col = loc.col + 1; row = loc.row - 1 }
      { col = loc.col - 1; row = loc.row + 1 } ]
    |> List.filter isLocationInsideBoard

/// Generate moves for Bishop
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

        // TODO: Check recursion in non tail position
        match isLocationInsideBoard newPos with
        | true -> newPos :: generateLine newPos dir
        | false -> []

    directions |> List.collect (generateLine loc)

/// Generate moves for Rook
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

        // TODO: Check recursion in non tail position
        match isLocationInsideBoard newPos with
        | true -> newPos :: generateLine newPos dir
        | false -> []

    directions |> List.collect (generateLine loc)

/// Generate moves for Queen
let generateQueenMove (loc: Location) =
    generateBishopMove loc @ generateRookMove loc

/// Generate moves based on the different pieces
let generateMoves (piece: ChessPiece) (loc: Location) =
    match piece with
    | ChessPiece.BlackPawn -> generatePawnMove loc Down
    | ChessPiece.WhitePawn -> generatePawnMove loc Up
    | ChessPiece.BlackRook
    | ChessPiece.WhiteRook -> generateRookMove loc
    | ChessPiece.BlackKnight
    | ChessPiece.WhiteKnight -> generateKnightMove loc
    | ChessPiece.BlackBishop
    | ChessPiece.WhiteBishop -> generateBishopMove loc
    | ChessPiece.BlackQueen
    | ChessPiece.WhiteQueen -> generateQueenMove loc
    | ChessPiece.BlackKing
    | ChessPiece.WhiteKing -> generateKingMove loc
    | _ -> []

/// Check if moves is valid and within the chessboard
let isValidMove (move1: string) (move2: string) =

    // NOTE: It does not consider chess pieces yet
    let isValidSquare (square: string) =
        square.Length = 2
        && square[0] >= 'a'
        && square[0] <= 'h'
        && square[1] >= '1'
        && square[1] <= '8'

    move1 <> move2 && isValidSquare move1 && isValidSquare move2

/// Get the location from the string representation, i.e. a1 -> {col = 0; row = 7 },
/// Note: rows are inverted, because in chess, the numbers start from bottom but in code, is represented
/// from bottom
let getSquare (move: string) =
    let col = int (move[0] - 'a')
    let row = 7 - int (move[1] - '1') // Flip the board upside down for array indexing
    { col = col; row = row }

/// Check if the move is valid
let validateMove (move1: string) (move2: string) (state: State) =
    match isValidMove move1 move2 with
    | true ->
        let fromLoc = getSquare move1
        let toLoc = getSquare move2

        let piece = state.board[fromLoc.row][fromLoc.col]

        // Check pieces to move corresponds to turn
        let isCorrectTurn =
            match piece with
            | ChessPiece.BlackPawn
            | ChessPiece.BlackRook
            | ChessPiece.BlackKnight
            | ChessPiece.BlackBishop
            | ChessPiece.BlackQueen
            | ChessPiece.BlackKing -> state.turn = Black
            | ChessPiece.WhitePawn
            | ChessPiece.WhiteRook
            | ChessPiece.WhiteKnight
            | ChessPiece.WhiteBishop
            | ChessPiece.WhiteQueen
            | ChessPiece.WhiteKing -> state.turn = White
            | _ -> false

        match isCorrectTurn with
        | true ->
            let moves = generateMoves piece fromLoc |> List.filter (fun loc -> loc = toLoc)

            // Filter out piece on the board, only applicable for pawn
            let moves =
                match piece with
                | ChessPiece.BlackPawn
                | ChessPiece.WhitePawn ->
                    moves
                    |> List.filter (fun loc -> state.board[loc.row][loc.col] = ChessPiece.Blank)
                | _ -> moves

            // If there are no moves generated, then it is not a valid move
            match moves.Length with
            | 0 ->
                printfn "No valid moves."
                false
            | _ ->
                printfn $"Moving %A{piece} from %s{move1} to %s{move2}"
                true
        | false ->
            printfn "Incorrect turn."
            false
    | false -> false

/// Parse the input string into a Command
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

/// Helper function to update the cells in the chessboard
let updateCell loc piece board =
    // If it reaches the loc to set, replace with piece, otherwise it copies the previous cell val
    let setVal r row c col cell =
        match r = row && c = col with
        | true -> piece
        | false -> cell

    board
    |> List.mapi (fun r rowList -> rowList |> List.mapi (fun c -> setVal r loc.row c loc.col))

/// Move the piece from one location to another
let movePiece (piece: ChessPiece) (move1: Location) (move2: Location) (board: ChessBoard) =
    // TODO: Check if move2 is not occupied
    board |> updateCell move1 ChessPiece.Blank |> updateCell move2 piece

/// Print the pieces to char
let pieceToChar (piece: ChessPiece) = piece |> int |> char

/// Helper to print the state of the chessboard
let printBoard (board: ChessBoard) =
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

/// Switch turn
let switchTurn turn =
    match turn with
    | Black -> White
    | White -> Black

/// Check if occupied space is a blank space
/// Check if Pawn has promotion or not
let hasPromotion piece dest =
    let lastRow = dest.row = 0 || dest.row = 8

    match piece with
    | ChessPiece.WhitePawn
    | ChessPiece.BlackPawn -> lastRow
    | _ -> false

let rec promotePawn turn =
    printf "Pawn promotion, choose piece to be promoted to Rook (r), Knight (k), Bishop (b), Queen (q): "
    let input = Console.ReadLine()
    // Make sure input is valid
    let trimmedInput = input.Trim().ToLower()

    match trimmedInput with
    | "r" ->
        printfn "Promoted to Rook."

        match turn with
        | Black -> ChessPiece.BlackRook
        | White -> ChessPiece.WhiteRook
    | "k" ->
        printfn "Promoted to Knight."

        match turn with
        | Black -> ChessPiece.BlackKnight
        | White -> ChessPiece.WhiteKnight
    | "b" ->
        printfn "Promoted to Bishop."

        match turn with
        | Black -> ChessPiece.BlackBishop
        | White -> ChessPiece.WhiteBishop
    | "q" ->
        printfn "Promoted to Queen."

        match turn with
        | Black -> ChessPiece.BlackQueen
        | White -> ChessPiece.WhiteQueen
    | _ ->
        printfn "Invalid promotion."
        promotePawn turn

/// Main game loop
let rec gameLoop (state: State) =
    state.board |> printBoard

    printfn $"\nIt is currently %A{state.turn}'s turn."
    printf "Enter move (e.g. 'e2 e4') or 'quit' to exit: "

    let restart state =
        Console.ReadLine() |> ignore
        state |> gameLoop

    let input = Console.ReadLine()
    let command = parseInput input

    // Validate moves
    let command =
        match command with
        | ChessMove(move1, move2) ->
            match state |> validateMove move1 move2 with
            | true -> ChessMove(move1, move2)
            | false -> Invalid
        | _ -> command

    // Proceed with moves and update board
    match command with
    | Quit -> printfn "Thank you for playing. Press enter to continue."
    | Invalid ->
        printfn "Invalid input, please try again."
        state |> restart
    | Castle castle -> state |> restart
    | ChessMove(move1, move2) ->
        // TODO: Check if moving to is occupied


        // Pawn promotion
        let start = getSquare move1
        let piece = state.board[start.row][start.col]

        match hasPromotion piece (getSquare move2) with
        | true ->
            let piece = promotePawn state.turn

            let state =
                { board = state.board |> movePiece piece (getSquare move1) (getSquare move2)
                  turn = switchTurn state.turn }

            state |> restart
        | false ->
            let start = getSquare move1
            let piece = state.board[start.row][start.col]

            let state =
                { board = state.board |> movePiece piece start (getSquare move2)
                  turn = switchTurn state.turn }

            state |> restart

/// Init the chess board
let initState () =
    let row = List.init 8 (fun _ -> ChessPiece.Blank)
    let blackPawns = List.init 8 (fun _ -> ChessPiece.BlackPawn)
    let whitePawns = List.init 8 (fun _ -> ChessPiece.WhitePawn)

    let board =
        [ [ ChessPiece.BlackRook
            ChessPiece.BlackKnight
            ChessPiece.BlackBishop
            ChessPiece.BlackQueen
            ChessPiece.BlackKing
            ChessPiece.BlackBishop
            ChessPiece.BlackKnight
            ChessPiece.BlackRook ]
          blackPawns
          row
          row
          row
          row
          whitePawns
          [ ChessPiece.WhiteRook
            ChessPiece.WhiteKnight
            ChessPiece.WhiteBishop
            ChessPiece.WhiteQueen
            ChessPiece.WhiteKing
            ChessPiece.WhiteBishop
            ChessPiece.WhiteKnight
            ChessPiece.WhiteRook ] ]

    { board = board; turn = White }

/// Start the game
let game () =
    let state = initState ()
    state |> gameLoop

// TODO: Make this as a test
/// For testing pawn promotion
let testPawnPromotion () =
    let state = initState ()

    let newState =
        { board =
            updateCell (getSquare "a8") ChessPiece.Blank state.board
            |> updateCell (getSquare "a7") ChessPiece.WhitePawn
          turn = White }

    newState |> gameLoop

game ()
// testPawnPromotion
