# Chess game in F#

Simple CLI app with chess rule implemented. There is no AI, just PvP. Player can make moves by typing in the locations and the engine will check if the moves are valid and move accordingly.

## Structure

Currently, the program are stored in `Chess` folder and tests are in `Chess.Tests`.

## Setup

```
dotnet new sln
# dotnet add package xunit
# dotnet add package xunit.runner.visualstudio
dotnet sln add Chess/Chess.fsprog
dotnet sln add Chess.Tests/Chess.Tests.fsprog
```

## Running application

```
dotnet run --project Chess
```

## Running test

```
dotnet test
```

## Features

- Parse inputs and validation of moves
- Piece moving
- Display board and updating of board
- Turn based
- Pawn promotion
- Capturing pieces
- En passant

## TODO
- Win condition
- Castling
- More tests