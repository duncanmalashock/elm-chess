module Piece exposing
    ( MovementRule(..)
    , Piece
    , bishop
    , king
    , knight
    , movementRules
    , pawn
    , queen
    , rook
    , square
    )

import Player
import Square


type Piece
    = Piece Player.Player PieceType (Maybe Square.Square)


square : Piece -> Maybe Square.Square
square (Piece _ _ maybeSquare) =
    maybeSquare


type PieceType
    = Pawn
    | Knight
    | Bishop
    | Rook
    | Queen
    | King


type MovementRule
    = MoveRule { stoppedByInterveningPieces : Bool } (List Square.Step)
    | CaptureRule { stoppedByInterveningPieces : Bool } (List Square.Step)


moveRules :
    { stoppedByInterveningPieces : Bool }
    -> List (List Square.Step)
    -> List MovementRule
moveRules { stoppedByInterveningPieces } moveSteps =
    List.map
        (\stepList ->
            MoveRule
                { stoppedByInterveningPieces =
                    stoppedByInterveningPieces
                }
                stepList
        )
        moveSteps


captureRules :
    { stoppedByInterveningPieces : Bool }
    -> List (List Square.Step)
    -> List MovementRule
captureRules { stoppedByInterveningPieces } moveSteps =
    List.map
        (\stepList ->
            CaptureRule
                { stoppedByInterveningPieces =
                    stoppedByInterveningPieces
                }
                stepList
        )
        moveSteps


unlimited : Square.Step -> List (List Square.Step)
unlimited step =
    [ List.repeat 1 step
    , List.repeat 2 step
    , List.repeat 3 step
    , List.repeat 4 step
    , List.repeat 5 step
    , List.repeat 6 step
    , List.repeat 7 step
    ]


unlimitedDiagonal : List (List Square.Step)
unlimitedDiagonal =
    List.concat
        [ unlimited Square.northwest
        , unlimited Square.northeast
        , unlimited Square.southwest
        , unlimited Square.southeast
        ]


unlimitedCardinal : List (List Square.Step)
unlimitedCardinal =
    List.concat
        [ unlimited Square.north
        , unlimited Square.east
        , unlimited Square.south
        , unlimited Square.west
        ]


kingMoves : List (List Square.Step)
kingMoves =
    [ [ Square.north ]
    , [ Square.east ]
    , [ Square.south ]
    , [ Square.west ]
    , [ Square.northwest ]
    , [ Square.northeast ]
    , [ Square.southwest ]
    , [ Square.southeast ]
    ]


knightMoves : List (List Square.Step)
knightMoves =
    [ [ Square.north
      , Square.north
      , Square.west
      ]
    , [ Square.north
      , Square.north
      , Square.east
      ]
    , [ Square.south
      , Square.south
      , Square.west
      ]
    , [ Square.south
      , Square.south
      , Square.east
      ]
    , [ Square.west
      , Square.west
      , Square.north
      ]
    , [ Square.west
      , Square.west
      , Square.south
      ]
    , [ Square.east
      , Square.east
      , Square.north
      ]
    , [ Square.east
      , Square.east
      , Square.south
      ]
    ]


movementRules : Piece -> List MovementRule
movementRules (Piece player pieceType maybeSquare) =
    case pieceType of
        Pawn ->
            case player of
                Player.White ->
                    List.concat
                        [ moveRules
                            { stoppedByInterveningPieces = True
                            }
                            [ [ Square.north
                              ]
                            , [ Square.north
                              , Square.north
                              ]
                            ]
                        , captureRules
                            { stoppedByInterveningPieces = True
                            }
                            [ [ Square.northwest
                              , Square.northeast
                              ]
                            ]
                        ]

                Player.Black ->
                    List.concat
                        [ moveRules
                            { stoppedByInterveningPieces = True
                            }
                            [ [ Square.south
                              ]
                            , [ Square.south
                              , Square.south
                              ]
                            ]
                        , captureRules
                            { stoppedByInterveningPieces = True
                            }
                            [ [ Square.southwest
                              , Square.southeast
                              ]
                            ]
                        ]

        Knight ->
            List.concat
                [ moveRules
                    { stoppedByInterveningPieces = False
                    }
                    knightMoves
                , captureRules
                    { stoppedByInterveningPieces = False
                    }
                    knightMoves
                ]

        Bishop ->
            List.concat
                [ moveRules
                    { stoppedByInterveningPieces = True
                    }
                    unlimitedDiagonal
                , captureRules
                    { stoppedByInterveningPieces = True
                    }
                    unlimitedDiagonal
                ]

        Rook ->
            List.concat
                [ moveRules
                    { stoppedByInterveningPieces = True
                    }
                    unlimitedCardinal
                , captureRules
                    { stoppedByInterveningPieces = True
                    }
                    unlimitedCardinal
                ]

        Queen ->
            List.concat
                [ moveRules
                    { stoppedByInterveningPieces = True
                    }
                    (List.concat
                        [ unlimitedCardinal
                        , unlimitedDiagonal
                        ]
                    )
                , captureRules
                    { stoppedByInterveningPieces = True
                    }
                    (List.concat
                        [ unlimitedCardinal
                        , unlimitedDiagonal
                        ]
                    )
                ]

        King ->
            List.concat
                [ moveRules
                    { stoppedByInterveningPieces = True
                    }
                    kingMoves
                , captureRules
                    { stoppedByInterveningPieces = True
                    }
                    kingMoves
                ]


pawn : Player.Player -> Square.Square -> Piece
pawn player theSquare =
    Piece player Pawn (Just theSquare)


rook : Player.Player -> Square.Square -> Piece
rook player theSquare =
    Piece player Rook (Just theSquare)


knight : Player.Player -> Square.Square -> Piece
knight player theSquare =
    Piece player Knight (Just theSquare)


bishop : Player.Player -> Square.Square -> Piece
bishop player theSquare =
    Piece player Bishop (Just theSquare)


king : Player.Player -> Square.Square -> Piece
king player theSquare =
    Piece player King (Just theSquare)


queen : Player.Player -> Square.Square -> Piece
queen player theSquare =
    Piece player Queen (Just theSquare)
