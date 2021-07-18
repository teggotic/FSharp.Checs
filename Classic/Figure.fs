module Chess.Classic.Figure

open System
open Chess.OptionalBuilder
open Chess
open Types
open Board
open Classic.Config
open Classic.Helpers

let private _getDirections fig moved =
    let getLines count =
        [| for x, y in List.zip [ 1; 0; -1; 0 ] [ 0; 1; 0; -1 ] -> MoveDirection(count, (x, y)) |]

    let getDiagonals count =
        [| for x in [ -1; 1 ] do
               for y in [ -1; 1 ] do
                   yield MoveDirection(count, (x, y)) |]

    match fig with
    | King -> (getLines 1, getDiagonals 1) ||> Array.append
    | Queen ->
        (getLines maxBoardSize, getDiagonals maxBoardSize)
        ||> Array.append
    | Rook -> getLines maxBoardSize
    | Bishop -> getDiagonals maxBoardSize
    | Knight ->
        [| for i in [ -2; 2 ] do
               for j in [ -1; 1 ] do
                   yield MoveDirection(1, (i, j))
                   yield MoveDirection(1, (j, i)) |]
    | Pawn -> [| MoveDirection((if moved then 1 else 2), (0, 1)) |]

let filterInbounds =
    let isActionInBounds (x, y) =
        x >= 0
        && x < boardWidth
        && y >= 0
        && y < boardHeight

    Array.filter isActionInBounds |> Array.map

let findCoordsBy f (cells: Cells) =
    [| for y in 0 .. boardHeight - 1 do
           for x in 0 .. boardWidth - 1 do
               if f cells.[y].[x] then yield (x, y) |]

open MoveDirection

module FigActions =
    let KingMovesAttacks =
        _getDirections King false |> getLineMoves

    let QueenMovesAttacks =
        _getDirections Queen false |> getLineMoves

    let BishopMovesAttacks =
        _getDirections Bishop false |> getLineMoves

    let RookMovesAttacks =
        _getDirections Rook false |> getLineMoves

    let KnightMovesAttacks =
        _getDirections Knight false |> getLineMoves

    let PawnMoves = _getDirections Pawn >> getLineMoves

[<RequireQualifiedAccess>]
module Moves =

    let get (figure: Figure) =
        match figure.Fig with
        | King -> FigActions.KingMovesAttacks
        | Queen -> FigActions.QueenMovesAttacks
        | Bishop -> FigActions.BishopMovesAttacks
        | Rook -> FigActions.RookMovesAttacks
        | Knight -> FigActions.KnightMovesAttacks

        | Pawn ->
            let pawnMoves = FigActions.PawnMoves figure.Moved

            if figure.Side = White then
                rotateY pawnMoves
            else
                pawnMoves

    let isValid (cells: Cells) moveTo =
        let destCell = getCellByCoords cells moveTo

        destCell.IsNone

    let filterPossible isValidMove moves =
        [| for movePack in moves do
               yield! (Array.takeWhile isValidMove movePack) |]

[<RequireQualifiedAccess>]
module Attacks =
    let get { Fig = fig; Side = side } =
        match fig with
        | King -> FigActions.KingMovesAttacks
        | Queen -> FigActions.QueenMovesAttacks
        | Bishop -> FigActions.BishopMovesAttacks
        | Rook -> FigActions.RookMovesAttacks
        | Knight -> FigActions.KnightMovesAttacks
        | Pawn ->
            let whiteAttacks = [| [| (-1, 1) |]; [| (1, 1) |] |]

            if side = White then
                rotateY whiteAttacks
            else
                whiteAttacks

    let isValid (cellSide: Side) (cells: Cells) moveTo =
        let destCell = getCellByCoords cells moveTo

        destCell
        |> Option.exists (fun { Side = destSide } -> destSide <> cellSide)

    let filterPossible isValidMove isValidAttack attacks =
        [| for attackPack in attacks do
               let possibleAttackOption =
                   Array.skipWhile isValidMove attackPack
                   |> Array.tryHead
                   |> Option.filter isValidAttack

               if possibleAttackOption.IsSome then
                   yield possibleAttackOption.Value |]

type GetPossibleAttacks = Coords -> Figure -> Cells -> Coords []

let checkActionForKingCheck (cells: Cells) (getPossibleAttacks: GetPossibleAttacks) fromCoords toCoords =
    // (let validateActualMove (board: Board) fromI toI =
    let cell = getCellByCoords cells fromCoords

    optional {
        let! cell = cell

        let newCells =
            [| for y in 0 .. boardHeight - 1 ->
                   [| for x in 0 .. boardWidth - 1 ->
                          if (x, y) = fromCoords then
                              None
                          else if (x, y) = toCoords then
                              getCellByCoords cells fromCoords
                              |> Option.map (fun x -> { x with Moved = true })
                          else
                              getCellByCoords cells (x, y) |] |]

        let attacks =
            [|
               for y in 0 .. boardHeight - 1 do
                   for x in 0 .. boardWidth - 1 do
                       let cell' = getCellByCoords newCells (x, y)
                       if cell'.IsSome && cell'.Value.Side <> cell.Side then
                           yield! getPossibleAttacks (x, y) cell'.Value newCells
                           |]

        let kingCoords =
            newCells
            |> findCoordsBy
                (function
                | Some { Side = side; Fig = King } -> side = cell.Side
                | _ -> false)
            |> Array.head

        return Array.contains kingCoords attacks |> not
    }
    |> Option.exists ((=) true)

let relativeActionsToGlobal (x, y) =
    Array.map (fun (xx, yy) -> (x + xx, y + yy))
    |> Array.map
