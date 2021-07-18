namespace Chess

open Types

type IChessRules =
    abstract member InitialBoard : Board
    abstract member GetActions : Board -> Coords -> Coords []
