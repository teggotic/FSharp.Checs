namespace Chess

open Chess


type Figure =
    | King
    | Queen
    | Bishop
    | Rook
    | Knight
    | Pawn



module Figure =
    let boardSize = 8

    let private _getDirections fig =
        let getLines count =
            [| for x, y in List.zip [ 1; 0; -1; 0 ] [ 0; 1; 0; -1 ] -> MoveDirection(count, (x, y)) |]

        let getDiagonals count =
            [| for x in [ -1; 1 ] do
                   for y in [ -1; 1 ] do
                       yield MoveDirection(count, (x, y)) |]

        match fig with
        | King -> (getLines 1, getDiagonals 1) ||> Array.append
        | Queen ->
            (getLines boardSize, getDiagonals boardSize)
            ||> Array.append
        | Rook -> getLines boardSize
        | Bishop -> getDiagonals boardSize
        | Knight ->
            [| for i in [ -2; 2 ] do
                for j in [ -1; 1 ] do
                    yield MoveDirection(1, (i, j))
               for i in [ -1; 1 ] do
                   for j in [ -2; 2 ] do
                       yield MoveDirection(1, (i, j)) |]
        | Pawn -> [| MoveDirection(1, (0, 1)) |]

    open MoveDirection

    // TODO: FINISH AND USE
    module FigMoves =
        let KingMoves = _getDirections King |> getLineMoves
        let QueenMoves = _getDirections Queen |> getLineMoves
        let BishopMoves = _getDirections Bishop |> getLineMoves
        let RookMoves = _getDirections Rook |> getLineMoves
        let KnightMoves = _getDirections Knight |> getLineMoves
        let PawnMoves = _getDirections Pawn |> getLineMoves

    let getDirections fig =
        match fig with
        | King -> (FigMoves.KingMoves, FigMoves.KingMoves)
        | Queen -> (FigMoves.QueenMoves, FigMoves.QueenMoves)
        | Bishop -> (FigMoves.BishopMoves, FigMoves.BishopMoves)
        | Rook -> (FigMoves.RookMoves, FigMoves.RookMoves)
        | Knight -> (FigMoves.KnightMoves, FigMoves.KnightMoves)

        | Pawn ->
            let attackMoves = [| [| (-1, 1) |]; [| (1, 1) |] |]
            (FigMoves.PawnMoves, attackMoves)


    let getIcon fig side = ()
