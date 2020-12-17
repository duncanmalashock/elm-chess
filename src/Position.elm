module Position exposing
    ( defaultSetup
    , init
    , legalMoves
    , moveToString
    , toString
    , tryMove
    )

import List.Extra
import Maybe.Extra
import Piece
import Player
import Set.Any
import Square


type Position
    = Position PositionDetails


type alias PositionDetails =
    { pieces : List Piece.Piece
    , initialSetup : List Piece.Piece
    , playerToMove : Player.Player
    , movesLeftUntilDraw : Int
    , enPassantSquare : Maybe Square.Square
    , castlingRights : Set.Any.AnySet Int Player.Player
    , error : Maybe Error
    }


legalMoves : Position -> List Move
legalMoves ((Position details) as thePosition) =
    List.concatMap (pieceMoves thePosition) details.pieces
        |> List.filter (moveIsLegal thePosition)


moveIsLegal : Position -> Move -> Bool
moveIsLegal thePosition theMove =
    validateMove thePosition theMove
        |> (\( _, maybeError ) ->
                maybeError == Nothing
           )


validateMove : Position -> Move -> ( Position, Maybe Error )
validateMove ((Position positionDetails) as thePosition) theMove =
    case theMove of
        Move piece { from, to } squaresTraveled ->
            if Piece.player piece /= positionDetails.playerToMove then
                ( thePosition, Just (IllegalMoveError theMove NotCurrentPlayersPiece) )

            else
                case Piece.square piece of
                    Just square ->
                        if squaresContainPieces squaresTraveled thePosition then
                            ( thePosition, Just (IllegalMoveError theMove PiecesBlockMovePath) )

                        else
                            ( thePosition, Nothing )

                    Nothing ->
                        ( thePosition, Just (IllegalMoveError theMove PieceNotOnBoard) )


squaresContainPieces : List Square.Square -> Position -> Bool
squaresContainPieces squares thePosition =
    List.filter
        (\s ->
            pieceAt s thePosition
                |> Maybe.Extra.isJust
        )
        squares
        |> List.isEmpty
        |> not


moveToString : Move -> String
moveToString theMove =
    case theMove of
        Move piece { from, to } squaresTraveled ->
            Piece.toString piece
                ++ Square.toString from
                ++ "-"
                ++ Square.toString to


pieceMoves : Position -> Piece.Piece -> List Move
pieceMoves thePosition thePiece =
    case Piece.square thePiece of
        Just startingSquare ->
            List.filterMap
                (\rule -> moveFromMovementRule thePiece startingSquare rule)
                (Piece.movementRules thePiece)

        Nothing ->
            []


moveFromMovementRule : Piece.Piece -> Square.Square -> Piece.MovementRule -> Maybe Move
moveFromMovementRule thePiece startingSquare movementRule =
    case movementRule of
        Piece.MoveRule { stoppedByInterveningPieces } steps ->
            moveFromMovementRuleHelp thePiece startingSquare steps

        Piece.CaptureRule { stoppedByInterveningPieces } steps ->
            moveFromMovementRuleHelp thePiece startingSquare steps


moveFromMovementRuleHelp : Piece.Piece -> Square.Square -> List Square.Step -> Maybe Move
moveFromMovementRuleHelp thePiece startingSquare steps =
    List.foldl
        (\step maybeSquare ->
            Maybe.andThen (Square.applyStep step) maybeSquare
        )
        (Just startingSquare)
        steps
        |> Maybe.map
            (\endingSquare ->
                Move thePiece
                    { from = startingSquare
                    , to = endingSquare
                    }
                    (squaresFromSteps steps startingSquare)
            )


squaresFromSteps : List Square.Step -> Square.Square -> List Square.Square
squaresFromSteps steps startingSquare =
    squaresFromStepsHelp steps startingSquare []


squaresFromStepsHelp : List Square.Step -> Square.Square -> List Square.Square -> List Square.Square
squaresFromStepsHelp steps startingSquare outputSquaresSoFar =
    case steps of
        [] ->
            outputSquaresSoFar

        head :: tail ->
            let
                newStartingSquare : Maybe Square.Square
                newStartingSquare =
                    Square.applyStep head startingSquare

                newOutputSquaresSoFar : List Square.Square
                newOutputSquaresSoFar =
                    Maybe.map (\sq -> [ sq ] ++ outputSquaresSoFar) newStartingSquare
                        |> Maybe.withDefault outputSquaresSoFar
            in
            squaresFromStepsHelp
                tail
                (newStartingSquare |> Maybe.withDefault startingSquare)
                newOutputSquaresSoFar


type Error
    = IllegalMoveError Move IllegalMoveReason


type IllegalMoveReason
    = PieceNotOnBoard
    | PiecesBlockMovePath
    | NotCurrentPlayersPiece


type Move
    = Move Piece.Piece { from : Square.Square, to : Square.Square } (List Square.Square)


tryMove : Move -> Position -> Position
tryMove theMove (Position positionDetails) =
    case theMove of
        Move piece { from, to } squaresTraveled ->
            { positionDetails
                | pieces =
                    List.Extra.setIf
                        (Piece.isAtSquare from)
                        (Piece.setSquare to piece)
                        positionDetails.pieces
            }
                |> Position


toString : Position -> String
toString ((Position positionDetails) as thePosition) =
    List.map
        (\rank ->
            List.map
                (\square ->
                    pieceAt square thePosition
                        |> Maybe.map Piece.toString
                        |> Maybe.withDefault " "
                )
                (Square.allOnRank rank)
                |> String.join ""
        )
        (List.reverse Square.allRanks)
        |> String.join "\n"


pieceAt : Square.Square -> Position -> Maybe Piece.Piece
pieceAt theSquare (Position positionDetails) =
    List.filter
        (Piece.isAtSquare theSquare)
        positionDetails.pieces
        |> List.head


init : List Piece.Piece -> Position
init initialSetup =
    Position
        { pieces = initialSetup
        , initialSetup = initialSetup
        , playerToMove = Player.White
        , movesLeftUntilDraw = 50
        , enPassantSquare = Nothing
        , castlingRights = Set.Any.fromList Player.playerToInt [ Player.White, Player.Black ]
        , error = Nothing
        }


defaultSetup : List Piece.Piece
defaultSetup =
    List.concat
        [ initPawns Player.Black
        , initPawns Player.White
        , initPieces Player.Black
        , initPieces Player.White
        ]


initPawns : Player.Player -> List Piece.Piece
initPawns player =
    let
        rank =
            case player of
                Player.White ->
                    Square.two

                Player.Black ->
                    Square.seven
    in
    [ Piece.pawn player (Square.square Square.a rank)
    , Piece.pawn player (Square.square Square.b rank)
    , Piece.pawn player (Square.square Square.c rank)
    , Piece.pawn player (Square.square Square.d rank)
    , Piece.pawn player (Square.square Square.e rank)
    , Piece.pawn player (Square.square Square.f rank)
    , Piece.pawn player (Square.square Square.g rank)
    , Piece.pawn player (Square.square Square.h rank)
    ]


initPieces : Player.Player -> List Piece.Piece
initPieces player =
    let
        rank =
            case player of
                Player.White ->
                    Square.one

                Player.Black ->
                    Square.eight
    in
    [ Piece.rook player (Square.square Square.a rank)
    , Piece.knight player (Square.square Square.b rank)
    , Piece.bishop player (Square.square Square.c rank)
    , Piece.king player (Square.square Square.d rank)
    , Piece.queen player (Square.square Square.e rank)
    , Piece.bishop player (Square.square Square.f rank)
    , Piece.knight player (Square.square Square.g rank)
    , Piece.rook player (Square.square Square.h rank)
    ]
