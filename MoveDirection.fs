namespace Chess

type MoveDirection =
    MoveDirection of count: int * direction: (int * int)

module MoveDirection =
    let getLineMoves directions =
        [|
            for MoveDirection (count, (x, y)) in directions ->
                [|
                    for i in 1..count -> (x * i, y * i)
                |]
        |]
