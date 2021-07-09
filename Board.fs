namespace Chess

open System
open System.Security.Cryptography
open System.Threading

type Board = { cells: Cell array }

module Board =

    let initialBoard =
        { cells =
              [| for i in 0 .. 63 ->
                     let side = if i < 16 then Black else White
                     Cell.getCellByIndex i side |] }

    let iToXY i = (i % 8, i / 8)
    let XYToI (x, y) = y * 8 + x

    let moveIsInBounds (x, y) = x >= 0 && x < 8 && y >= 0 && y < 8
    
    let isValidMove (board: Board) moveTo =
        if moveIsInBounds moveTo then
            let destCell = board.cells.[XYToI moveTo]
            destCell = EmptyCell
        else
            false
    
    let isValidAttack cell (board: Board) moveTo =
        if moveIsInBounds moveTo then
            let destCell = board.cells.[XYToI moveTo]
            match destCell, cell with
            | Cell {side = destSide}, Cell {side = side} ->
                destSide <> side
            | _ ->
                false
        else
            false
            
    let getValidMoves f movePacks =
        [| for movePack in movePacks do
               yield! (Array.takeWhile f movePack) |]
        
    let getValidAttacks f g movePacks =
        [| for movePack in movePacks do
               let validMoves = Array.skipWhile g movePack
               if Array.isEmpty validMoves |> not && f validMoves.[0] then
                   yield validMoves.[0] |]

    let getPossibleMoves (board: Board) cellI =
        let cell = board.cells.[cellI]
        let _isValidMove = isValidMove board
        let _isValidAttack = isValidAttack cell board
        let coords = iToXY cellI
        Cell.getAllMoves cell
        |> Option.map
            (fun (moves, attacks) ->
                let validMoves =
                    (coords, moves)
                    ||> CoordUtils.relativeToGlobal
                    |> getValidMoves _isValidMove
                    
                let validAttacks =
                    (coords, attacks)
                    ||> CoordUtils.relativeToGlobal
                    |> getValidAttacks _isValidAttack _isValidMove
                    
                (validMoves, validAttacks)
                )

    let getBoardAttacks f (board: Board) =
        let possibleMovePacks =
            (Seq.zip board.cells [0..63] )
            |> Seq.filter f 
            |> Seq.map (fun (_, i) ->
                    getPossibleMoves board i
                    |> Option.map (fun (_, attacks) -> attacks )
                )
            |> Seq.toArray
        [|
            for movePack in possibleMovePacks do
                if movePack.IsSome then
                    yield! movePack.Value
        |]
        
    let validateActualMove (board: Board) fromI toI =
        let cell = board.cells.[fromI]
        
        match cell with
        | EmptyCell ->
            false
        | Cell {side = side} ->
            let newBoard = {
                cells = [|
                    for i in 0..63 ->
                        if i = fromI then
                            EmptyCell
                        else if i = toI then
                            board.cells.[fromI]
                        else
                            board.cells.[i]
                |]
            }
        
            let attacks =
                newBoard
                |> getBoardAttacks (function
                    | Cell {side = toSide}, _ ->
                        side <> toSide
                    | _ -> false )
            
            let kingLocation =
                newBoard.cells
                |> Array.zip [|0..63|]
                |> Array.find (function
                    | _, Cell cellState -> cellState = {side = side; figure = King}
                    | _ -> false
                    )
                |> fst
                |> iToXY
                
            attacks
            |> Array.contains kingLocation
            |> not
