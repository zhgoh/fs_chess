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
type ChessBoard = ChessPiece[,]

/// Game state to store board and turn info
type State = { board: ChessBoard; turn: Turn }

/// Location is just a record of positions on the board
type Location = { col: int; row: int }

/// Possible states of the game determined by inputs
type Command =
    | Quit
    | ChessMove of Location * Location
    | Castle of string
    | Invalid

/// Check if location is within the chessboard
let isLocationInsideBoard loc =
    loc.row >= 0 && loc.row < 8 && loc.col >= 0 && loc.col < 8

/// Generate moves for Pawn based on the direction it is facing
let generatePawnMove (board: ChessBoard) (loc: Location) (piece: ChessPiece) (dir: PawnDirection) =
    //TODO: Generate en passe moves
    let offset =
        match dir with
        | Up -> -1
        | Down -> 1

    let hasPiece loc =
        board[loc.row, loc.col] <> ChessPiece.Blank

    // Determine if pawn can jump 2 steps in front, if pieces blocking, it cannot jump in front
    let start =
        match loc with
        | { row = 1 }
        | { row = 6 } ->
            let front =
                { col = loc.col
                  row = loc.row + offset }

            match hasPiece front with
            | true -> []
            | false ->
                [ { col = loc.col
                    row = loc.row + offset + offset } ]
        | _ -> []

    let res =
        [ { col = loc.col
            row = loc.row + offset } ]
        @ start
        |> List.filter isLocationInsideBoard
        |> List.filter (fun loc -> board[loc.row, loc.col] = ChessPiece.Blank) // Only for pawn, filter out moves if pieces blocking

    let diagonals =
        [ { col = loc.col - 1
            row = loc.row + offset }
          { col = loc.col + 1
            row = loc.row + offset } ]
        |> List.filter isLocationInsideBoard

    let opponents =
        match piece with
        | ChessPiece.BlackPawn ->

            [ ChessPiece.WhitePawn
              ChessPiece.WhiteRook
              ChessPiece.WhiteKnight
              ChessPiece.WhiteBishop
              ChessPiece.WhiteQueen
              ChessPiece.WhiteKing ]
        | ChessPiece.WhitePawn ->
            [ ChessPiece.BlackPawn
              ChessPiece.BlackRook
              ChessPiece.BlackKnight
              ChessPiece.BlackBishop
              ChessPiece.BlackQueen
              ChessPiece.BlackKing ]
        | _ -> []

    let diagonals =
        diagonals
        |> List.filter (fun loc -> opponents |> List.contains (board[loc.row, loc.col]))

    res @ diagonals

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
let generateMoves (board: ChessBoard) (piece: ChessPiece) (loc: Location) =
    match piece with
    | ChessPiece.BlackPawn -> generatePawnMove board loc ChessPiece.BlackPawn Down
    | ChessPiece.WhitePawn -> generatePawnMove board loc ChessPiece.WhitePawn Up
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
let isValidMove (move1: Location) (move2: Location) =
    move1 <> move2 && isLocationInsideBoard move1 && isLocationInsideBoard move2

/// Get the location from the string representation, i.e. a1 -> {col = 0; row = 7 },
/// Note: rows are inverted, because in chess, the numbers start from bottom but in code, is represented
/// from bottom
let getSquare (move: string) =
    let col = int (move[0] - 'a')
    let row = 7 - int (move[1] - '1') // Flip the board upside down for array indexing
    // printfn $"Col: %d{col} Row: %d{row}"
    { col = col; row = row }

/// Check if the turn is correct and the moves generated is valid
let validateMove (move1: Location) (move2: Location) (state: State) =
    match isValidMove move1 move2 with
    | false ->
        printf "Move was invalid. Please try another move."
        false
    | true ->
        let piece = state.board[move1.row, move1.col]

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
        | false ->
            printf "Incorrect turn."
            false
        | true ->
            // Get all the moves and only filter by the end location
            let moves =
                generateMoves state.board piece move1 |> List.filter (fun loc -> loc = move2)

            // If there are no moves generated, then it is not a valid move
            match moves.Length with
            | 0 ->
                printf "Not a valid move for the piece."
                false
            | _ -> true

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
        | 2 -> ChessMove(getSquare parts[0], getSquare parts[1])
        | _ -> Invalid
    | _ -> Invalid

/// Helper function to update the cells in the chessboard
let placePiece loc piece board =
    // If it reaches the loc to set, replace with piece, otherwise it copies the previous cell val
    let newBoard = Array2D.copy board
    newBoard[loc.row, loc.col] <- piece
    newBoard

/// Move the piece from one location to another
let movePiece (piece: ChessPiece) (move1: Location) (move2: Location) (board: ChessBoard) =
    // TODO: Check if move2 is not occupied
    board |> placePiece move1 ChessPiece.Blank |> placePiece move2 piece

/// Print the pieces to char
let pieceToChar (piece: ChessPiece) = piece |> int |> char

/// Helper to print the state of the chessboard
let printBoard (board: ChessBoard) =
    Console.Write "\u001bc\x1b[3J"
    let boardCol = Array2D.length2 board - 1
    let blankSpacesWidth = boardCol * 2 + 2

    // Print top column headers (a, b, c, ...)
    printf "   " // Space for row numbers
    [ 0..boardCol ] |> Seq.iter (fun j -> printf $"%c{char (int 'a' + j)} ")

    // Print blank lines
    printf "\n  "

    [ 0..blankSpacesWidth ] |> Seq.iter (fun _ -> printf "─")

    printfn ""

    // Print rows with numbers (8, 7, 6, ...)
    [ 0 .. Array2D.length1 board - 1 ]
    |> Seq.iter (fun i ->
        printf $"%d{Array2D.length1 board - i}| " // Row number (reversed for chess)

        [ 0..boardCol ]
        |> Seq.map (fun j -> board.[i, j])
        |> Seq.iter (fun c -> printf $"%c{pieceToChar c} ")

        printfn "")

    // Print blank lines
    printf "  " // Space for Blank lines
    [ 0..blankSpacesWidth ] |> Seq.iter (fun _ -> printf "─")

    // Print A to Z
    printf "\n   "
    [ 0..boardCol ] |> Seq.iter (fun j -> printf $"%c{char (int 'a' + j)} ")

/// Switch turn
let switchTurn turn =
    match turn with
    | Black -> White
    | White -> Black

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

    let restart state = state |> gameLoop

    let input = Console.ReadLine()
    let command = parseInput input

    let invalidInput () =
        // printfn "Invalid input, please try again."
        Console.ReadLine() |> ignore
        state |> restart

    // Proceed with moves and update board
    match command with
    | Quit -> printfn "Thank you for playing. Press enter to continue."
    | Invalid -> invalidInput ()
    | Castle castle -> state |> restart
    | ChessMove(move1, move2) ->
        // Validate movements
        let validMovements = state |> validateMove move1 move2

        match validMovements with
        | false -> invalidInput ()
        | true ->
            // Pawn promotion
            let piece = state.board[move1.row, move1.col]
            let hasPawnPromoted = hasPromotion piece move2

            match hasPawnPromoted with
            | true ->
                let piece = promotePawn state.turn

                let state =
                    { board = state.board |> movePiece piece move1 move2
                      turn = switchTurn state.turn }

                state |> restart
            | false ->
                let piece = state.board[move1.row, move1.col]

                let state =
                    { board = state.board |> movePiece piece move1 move2
                      turn = switchTurn state.turn }

                state |> restart

/// Init the chess board
let initState () =
    let board =
        Array2D.create 8 8 ChessPiece.Blank
        |> placePiece (getSquare "a7") ChessPiece.BlackPawn
        |> placePiece (getSquare "b7") ChessPiece.BlackPawn
        |> placePiece (getSquare "c7") ChessPiece.BlackPawn
        |> placePiece (getSquare "d7") ChessPiece.BlackPawn
        |> placePiece (getSquare "e7") ChessPiece.BlackPawn
        |> placePiece (getSquare "f7") ChessPiece.BlackPawn
        |> placePiece (getSquare "g7") ChessPiece.BlackPawn
        |> placePiece (getSquare "h7") ChessPiece.BlackPawn
        |> placePiece (getSquare "a8") ChessPiece.BlackRook
        |> placePiece (getSquare "b8") ChessPiece.BlackKnight
        |> placePiece (getSquare "c8") ChessPiece.BlackBishop
        |> placePiece (getSquare "d8") ChessPiece.BlackQueen
        |> placePiece (getSquare "e8") ChessPiece.BlackKing
        |> placePiece (getSquare "f8") ChessPiece.BlackBishop
        |> placePiece (getSquare "g8") ChessPiece.BlackKnight
        |> placePiece (getSquare "h8") ChessPiece.BlackRook

        |> placePiece (getSquare "a2") ChessPiece.WhitePawn
        |> placePiece (getSquare "b2") ChessPiece.WhitePawn
        |> placePiece (getSquare "c2") ChessPiece.WhitePawn
        |> placePiece (getSquare "d2") ChessPiece.WhitePawn
        |> placePiece (getSquare "e2") ChessPiece.WhitePawn
        |> placePiece (getSquare "f2") ChessPiece.WhitePawn
        |> placePiece (getSquare "g2") ChessPiece.WhitePawn
        |> placePiece (getSquare "h2") ChessPiece.WhitePawn
        |> placePiece (getSquare "a1") ChessPiece.WhiteRook
        |> placePiece (getSquare "b1") ChessPiece.WhiteKnight
        |> placePiece (getSquare "c1") ChessPiece.WhiteBishop
        |> placePiece (getSquare "d1") ChessPiece.WhiteQueen
        |> placePiece (getSquare "e1") ChessPiece.WhiteKing
        |> placePiece (getSquare "f1") ChessPiece.WhiteBishop
        |> placePiece (getSquare "g1") ChessPiece.WhiteKnight
        |> placePiece (getSquare "h1") ChessPiece.WhiteRook


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
            placePiece (getSquare "a8") ChessPiece.Blank state.board
            |> placePiece (getSquare "a7") ChessPiece.WhitePawn
          turn = White }

    newState |> gameLoop

game ()
// testPawnPromotion
