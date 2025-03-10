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

let validateMove (inputs: list<string>) =
    let inputs =
        inputs
        |> List.filter (fun x -> x.Length = 2 && Char.IsAsciiLetterLower x[0] && Char.IsAsciiDigit x[1])

    match inputs.Length with
    | 2 -> true
    | _ -> false

let getMoves (input: string) =
    // Make sure input is valid
    let parts = input.Split ' '

    match parts.Length with
    | 1 ->
        match parts.[0] with
        | "0-0"
        | "0-0-0" -> Some input
        | _ -> None
    | _ ->
        match validateMove [ parts.[0]; parts.[1] ] with
        | true -> Some input
        | false -> None

// Main game loop
let rec gameLoop board =
    printBoard board
    printf "\nEnter move (e.g. 'e2 e4') or 'quit' to exit: "

    let input = Console.ReadLine()

    match input.ToLower() with
    | "quit"
    | "q" -> printfn "Thanks for playing!"
    | _ ->
        match String.IsNullOrWhiteSpace input with
        | true ->
            printfn "Invalid input, type in something."
            Console.ReadLine() |> ignore
        | false ->
            match getMoves input with
            | None ->
                printf "Invalid moves, please try again. Press enter to continue."
                Console.ReadLine() |> ignore
            | Some _ ->
                printfn "%s is a valid moves" input
                Console.ReadLine() |> ignore

            gameLoop board

// Start the game
let board = initBoard
gameLoop board
