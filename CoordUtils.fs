module Chess.CoordUtils
    let relativeToGlobal (x, y) movePacks  =
        ( Array.map (fun (xx, yy) -> (x + xx, y + yy)),
          movePacks )
        ||> Array.map 
