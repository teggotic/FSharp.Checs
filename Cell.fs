namespace Chess

open Chess

type CellState = {
    side: Side
    figure: Figure
}

module CellState =
    let getIconPath (cellState: CellState) =
        let figName = cellState.figure.ToString().ToLower()
        let sideName = cellState.side.ToString().ToLower()
        $"avares://Chess/Assets/icons/{figName}-{sideName}.png" 

type Cell =
    | EmptyCell
    | Cell of CellState
    
module Cell =
    let getCellByIndex i side =
        let fig =
            if i >= 16 && i < 64 - 8 - 8 then
                None
            else if i >= 8 && i < 16 then
                Some Pawn
            else if i >= 64 - 8 - 8 && i < 64 - 8 then
                Some Pawn
            else
                match i % 8 with
                | 0
                | 7 -> Some Rook
                | 1
                | 6 -> Some Knight
                | 2
                | 5 -> Some Bishop
                | 4 -> Some King
                | 3 -> Some Queen
                | _ -> failwith ""
        match fig with
        | Some f ->
            Cell {side = side; figure = f}
        | None ->
            EmptyCell
            
    let getIconPath cell =
        match cell with
        | EmptyCell ->
            None
        | Cell cellState ->
            CellState.getIconPath cellState
            |> Some
            
    // let getIconPath = function
    //     | EmptyCell ->
    //         None
    //     | Cell {side = side; figure = fig} ->
    //         let iconName =
    //             match fig with
    //             | King -> "k"
    //             | Queen -> "q"
    //             | Bishop -> "b"
    //             | Rook -> "r"
    //             | Knight -> "kn"
    //             | Pawn -> "p"
    //         let sideChar =
    //             match side with
    //             | White -> "w"
    //             | Black -> "b"
    //         Some $"icons/{iconName}{sideChar}.png"
        
    let getAllMoves = function
        | EmptyCell ->
            None
        | Cell {side = side; figure = fig} ->
            let rotateY =
                Array.map (fun (x, y) -> (x, -y))
                |> Array.map
            let moves, attacks = Figure.getDirections fig
                
            let moves =
                if fig = Pawn && side = White then
                    rotateY moves
                else
                    moves
                   
            let attacks =
                if fig = Pawn && side = White then
                    rotateY attacks
                else
                   attacks
                
            Some (moves, attacks)