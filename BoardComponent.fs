module Chess.BoardComponent

open System
open Avalonia
open Avalonia.Controls
open Avalonia.FuncUI.DSL

open Avalonia.Controls.Primitives
open Avalonia.Media.Imaging
open Avalonia.Platform
open Chess
open Types
open Board

type SelectedCell =
    { coords: int * int
      actions: (int * int) [] }

type State =
    { side: Side
      selectedCell: SelectedCell option
      board: Board
      rules: IChessRules }

let init =
    let rules = Classic.Rules() :> IChessRules

    { side = White
      selectedCell = None
      rules = rules
      board = rules.InitialBoard }

type Msg = CellClicked of (int * int)

let update (msg: Msg) (state: State) : State =
    let trySelectNewCell newCoords =
        let cell =
            getCellByCoords state.board.Cells newCoords

        match cell with
        | None -> state
        | Some figure ->
            if figure.Side = state.side then

                let actions =
                    state.rules.GetActions state.board newCoords

                let selectedCell =
                    { coords = newCoords
                      actions = actions }

                { state with
                      selectedCell = Some selectedCell }
            else
                state

    let tryMove positions selectedCell newCoords =
        if Array.contains newCoords positions then
            { state with
                  selectedCell = None
                  side =
                      if state.side = White then
                          Black
                      else
                          White
                  board =
                      { Cells =
                            [| for y in 0 .. 7 ->
                                   [| for x in 0 .. 7 ->
                                          if (x, y) = selectedCell.coords then
                                              None
                                          else if (x, y) = newCoords then
                                              getCellByCoords state.board.Cells selectedCell.coords
                                              |> Option.map (fun x -> {x with Moved = true})
                                          else
                                              getCellByCoords state.board.Cells (x, y) |] |] } }
        else
            state

    match msg with
    | CellClicked newCoords ->
        let cell =
            getCellByCoords state.board.Cells newCoords

        match state.selectedCell with
        | Some selectedCell ->
            if newCoords = selectedCell.coords then
                { state with selectedCell = None }
            else
                match cell with
                | Some { Side = side } ->
                    if side = state.side then
                        trySelectNewCell newCoords
                    else
                        tryMove selectedCell.actions selectedCell newCoords
                | None -> tryMove selectedCell.actions selectedCell newCoords
        | None -> trySelectNewCell newCoords

let assets =
    AvaloniaLocator.Current.GetService<IAssetLoader>()

let view (state: State) (dispatch) =

    UniformGrid.create [ UniformGrid.columns 8
                         UniformGrid.rows 8
                         UniformGrid.children (
                             [ for y in 0 .. 7 do
                                   for x in 0 .. 7 do
                                       let attrs =
                                           [ Button.margin 5.0
                                             Button.onClick (fun _ -> dispatch (CellClicked(x, y))) ]

                                       let cell = getCellByCoords state.board.Cells (x, y)

                                       let attrs =
                                           match cell with
                                           | None -> attrs
                                           | Some fig ->
                                               [
                                                 // Button.content $"{cellState.figure}"
                                                 // Button.foreground $"{cellState.side}"
                                                 // let assetList = assets.GetAssets(Uri("avares://Chess"), Uri("avares://icons"))
                                                 // assetList
                                                 // |> Seq.toArray
                                                 // |> printfn "%A"
                                                 Button.content (
                                                     Image.create [ new Bitmap(assets.Open(Uri(Figure.getIconPath fig)))
                                                                    |> Image.source ]
                                                 ) ]
                                               |> List.append attrs

                                       let attrs =
                                           state.selectedCell
                                           |> Option.map
                                               (fun selectedCell ->
                                                   let attrs =
                                                       if selectedCell.coords = (x, y) then
                                                           [ Button.borderBrush "green"
                                                             Button.borderThickness 4.0 ]
                                                           |> List.append attrs
                                                       else
                                                           attrs

                                                   let attrs =
                                                       if Array.contains (x, y) selectedCell.actions then
                                                           (if (getCellByCoords state.board.Cells (x, y)).IsNone then
                                                                [ Button.borderBrush "blue"
                                                                  Button.borderThickness 4.0 ]
                                                            else
                                                                [ Button.borderBrush "red"
                                                                  Button.borderThickness 4.0 ])
                                                           |> List.append attrs
                                                       else
                                                           attrs

                                                   attrs)
                                           |> Option.defaultValue attrs


                                       Button.create attrs ]
                         ) ]
