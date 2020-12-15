module Player exposing
    ( Player(..)
    , playerToInt
    )


type Player
    = White
    | Black


playerToInt : Player -> Int
playerToInt player =
    case player of
        White ->
            0

        Black ->
            1
