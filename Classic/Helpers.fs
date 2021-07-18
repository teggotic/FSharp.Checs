module Chess.Classic.Helpers

open Chess.Types

let rotateY =
    Array.map (fun (x: int, y) -> (x, -y))
    |> Array.map
