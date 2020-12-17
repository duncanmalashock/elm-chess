module Player exposing
    ( Player(..)
    , opponent
    , playerToInt
    )


type Player
    = White
    | Black


opponent : Player -> Player
opponent player =
    case player of
        White ->
            Black

        Black ->
            White


playerToInt : Player -> Int
playerToInt player =
    case player of
        White ->
            0

        Black ->
            1
