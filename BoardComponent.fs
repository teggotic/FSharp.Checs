module Chess.BoardComponent

open System
open Avalonia
open Avalonia.Controls
open Avalonia.FuncUI.DSL

open Avalonia.Controls.Primitives
open Avalonia.Media.Imaging
open Avalonia.Platform
open Chess
open Board

type SelectedCell =
    { i: int
      cellState: CellState
      moves: int []
      attacks: int [] }

type State =
    { side: Side
      selectedCell: SelectedCell option
      board: Board }

let init =
    { side = White
      selectedCell = None
      board = Board.initialBoard }

type Msg = CellClicked of int

let update (msg: Msg) (state: State) : State =
    let trySelectNewCell newI =
        let cell = state.board.cells.[newI]

        match cell with
        | EmptyCell -> state
        | Cell cellState ->
            if cellState.side = state.side then

                let (almostValidMoves, almostValidAttacks) =
                    (getPossibleMoves state.board newI).Value

                
                    // let tmp =
                    //     XYToI (7, 2)
                    //     |> validateActualMove state.board (newI)
                    // ()
                
                let validMoves =
                    almostValidMoves
                    |> Array.map XYToI
                    |> Array.filter (validateActualMove state.board newI)

                let validAttacks =
                    almostValidAttacks
                    |> Array.map XYToI
                    |> Array.filter (validateActualMove state.board newI)
                

                let selectedCell =
                    { i = newI
                      cellState = cellState
                      moves = validMoves
                      attacks = validAttacks }

                { state with
                      selectedCell = Some selectedCell }
            else
                state

    let tryMove positions selectedCell newI =
        if Array.contains newI positions then
            { state with
                  selectedCell = None
                  side =
                      if state.side = White then
                        Black
                      else
                        White
                  board =
                      { cells =
                            [| for i in 0 .. 63 ->
                                   if i = selectedCell.i then
                                       EmptyCell
                                   else if i = newI then
                                       state.board.cells.[selectedCell.i]
                                   else
                                       state.board.cells.[i] |] } }
        else
            state

    match msg with
    | CellClicked newI ->
        let cell = state.board.cells.[newI]

        match state.selectedCell with
        | Some selectedCell ->
            if newI = selectedCell.i then
                { state with selectedCell = None }
            else
                match cell with
                | Cell { side = side } ->
                    if side = state.side then
                        trySelectNewCell newI
                    else
                        tryMove selectedCell.attacks selectedCell newI
                | EmptyCell -> tryMove selectedCell.moves selectedCell newI
        | None -> trySelectNewCell newI

let assets =
    AvaloniaLocator.Current.GetService<IAssetLoader>()

let view (state: State) (dispatch) =

    UniformGrid.create [ UniformGrid.columns 8
                         UniformGrid.rows 8
                         UniformGrid.children (
                             [ for i in 0 .. 63 ->
                                   // let x = i % 8
                                   // let y = i / 8
                                   let attrs =
                                       [ Button.margin 5.0
                                         Button.onClick (fun _ -> dispatch (CellClicked i)) ]

                                   let cell = state.board.cells.[i]

                                   let attrs =
                                       match cell with
                                       | EmptyCell -> attrs
                                       | Cell cellState ->
                                           [
                                             // Button.content $"{cellState.figure}"
                                             // Button.foreground $"{cellState.side}"
                                             // let assetList = assets.GetAssets(Uri("avares://Chess"), Uri("avares://icons"))
                                             // assetList
                                             // |> Seq.toArray
                                             // |> printfn "%A"
                                             Button.content (
                                                 Image.create [ new Bitmap(
                                                                    assets.Open(Uri(CellState.getIconPath cellState))
                                                                )
                                                                |> Image.source ]
                                             ) ]
                                           |> List.append attrs

                                   let attrs =
                                       state.selectedCell
                                       |> Option.map
                                           (fun selectedCell ->
                                               let attrs =
                                                   if selectedCell.i = i then
                                                       [ Button.borderBrush "green"
                                                         Button.borderThickness 4.0 ]
                                                       |> List.append attrs
                                                   else
                                                       attrs

                                               let attrs =
                                                   if Array.contains i selectedCell.moves then
                                                       [ Button.borderBrush "blue"
                                                         Button.borderThickness 4.0 ]
                                                       |> List.append attrs
                                                   else
                                                       attrs

                                               let attrs =
                                                   if Array.contains i selectedCell.attacks then
                                                       [ Button.borderBrush "red"
                                                         Button.borderThickness 4.0 ]
                                                       |> List.append attrs
                                                   else
                                                       attrs

                                               attrs)
                                       |> Option.defaultValue attrs


                                   Button.create attrs ]
                         ) ]
