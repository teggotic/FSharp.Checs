namespace Chess.Classic

open Chess
open Types
open Board

open Classic.Config
open Classic.Figure

open Chess.OptionalBuilder

type Rules() =
    interface IChessRules with
        member _.GetActions board coords =
            let cell = getCellByCoords board.Cells coords

            let getPossibleMoves coords cell cells =
                let isValidMove' = Moves.isValid cells

                cell
                |> Moves.get
                |> relativeActionsToGlobal coords
                |> filterInbounds
                |> Moves.filterPossible isValidMove'

            let getPossibleAttacks coords cell cells =
                let isValidMove' = Moves.isValid cells
                let isValidAttack' = Attacks.isValid cell.Side cells

                cell
                |> Attacks.get
                |> relativeActionsToGlobal coords
                |> filterInbounds
                |> Attacks.filterPossible isValidMove' isValidAttack'

            let possibleActions =
                optional {
                    let! cell = cell
                    let possibleMoves = getPossibleMoves coords cell board.Cells

                    let possibleAttacks = getPossibleAttacks coords cell board.Cells

                    return
                        Array.append possibleMoves possibleAttacks
                        |> Array.filter (checkActionForKingCheck board.Cells getPossibleAttacks coords)
                }

            possibleActions |> Option.defaultValue [||]

        member _.InitialBoard =
            let emptyRow = Array.create boardWidth None

            let homeRow side =
                let rook = Figure.initial Rook side
                let knight = Figure.initial Knight side
                let bishop = Figure.initial Bishop side
                let queen = Figure.initial Queen side
                let king = Figure.initial King side

                [| rook
                   knight
                   bishop
                   queen
                   king
                   bishop
                   knight
                   rook |]
                |> Array.map Some

            let pawnRow side =
                Figure.initial Pawn side
                |> Some
                |> Array.create boardWidth

            let rowGroups =
                [ [| homeRow Black; pawnRow Black |]
                  Array.create (boardHeight - 4) emptyRow
                  [| pawnRow White; homeRow White |] ]

            { Cells = rowGroups |> List.fold Array.append [||] }
