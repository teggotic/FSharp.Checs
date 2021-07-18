module Chess.Types

type Side =
    | White
    | Black

type FigureType =
    | King
    | Queen
    | Bishop
    | Rook
    | Knight
    | Pawn

type Figure =
    { Fig: FigureType
      Side: Side
      Moved: bool }

module Figure =
    let initial fig side =
        { Fig = fig
          Side = side
          Moved = false }

    let getIconPath (figure: Figure) =
        let figName = figure.Fig.ToString().ToLower()
        let sideName = figure.Side.ToString().ToLower()
        $"avares://Chess/Assets/icons/{figName}-{sideName}.png"

type Coords = int * int

type Cells = Figure option [] []

type Board = { Cells: Cells }

module Board =
    let getCellByCoords (cells: Cells) (x, y) = cells.[y].[x]
