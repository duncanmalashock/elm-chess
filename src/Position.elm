module Position exposing
    ( Error(..)
    , IllegalMoveReason(..)
    , defaultSetup
    , init
    , lastError
    , legalMoves
    , pieceAt
    , play
    , toString
    )

import List.Extra
import Maybe.Extra
import Move
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


play : { from : Square.Square, to : Square.Square } -> Position -> Position
play { from, to } ((Position positionDetails) as thePosition) =
    case pieceAt from thePosition of
        Nothing ->
            { positionDetails
                | error =
                    Just <|
                        IllegalMoveError <|
                            NoPieceFoundAtSquare from
            }
                |> Position

        Just piece ->
            case findInAllMoves { from = from, to = to } thePosition of
                Nothing ->
                    { positionDetails
                        | error =
                            Just <|
                                IllegalMoveError <|
                                    ViolatesMovementRulesOfPiece (Piece.pieceType piece)
                    }
                        |> Position

                Just move ->
                    updatePosition move thePosition


updatePosition : Move.Move -> Position -> Position
updatePosition theMove ((Position positionDetails) as thePosition) =
    case validateMove thePosition theMove of
        Nothing ->
            case theMove of
                Move.Move piece { from, to } _ ->
                    { positionDetails
                        | pieces =
                            List.Extra.setIf
                                (Piece.isAtSquare from)
                                (Piece.setSquare to piece)
                                positionDetails.pieces
                        , error = Nothing
                        , playerToMove =
                            Player.opponent positionDetails.playerToMove
                    }
                        |> Position

        Just error ->
            { positionDetails
                | error =
                    Just error
            }
                |> Position


lastError : Position -> Maybe Error
lastError ((Position positionDetails) as thePosition) =
    positionDetails.error


findInAllMoves : { from : Square.Square, to : Square.Square } -> Position -> Maybe Move.Move
findInAllMoves { from, to } ((Position positionDetails) as thePosition) =
    List.concatMap Move.movesForPiece positionDetails.pieces
        |> List.filter
            (\move ->
                (Move.to move == to)
                    && (Move.from move == from)
            )
        |> List.head


legalMoves : Position -> List Move.Move
legalMoves ((Position positionDetails) as thePosition) =
    List.concatMap Move.movesForPiece positionDetails.pieces
        |> List.filter (\move -> validateMove thePosition move == Nothing)


validateMove : Position -> Move.Move -> Maybe Error
validateMove ((Position positionDetails) as thePosition) theMove =
    case theMove of
        Move.Move piece { from, to, capture, jumpsAllowed } squaresTraveled ->
            if Piece.player piece /= positionDetails.playerToMove then
                Just (IllegalMoveError NotCurrentPlayersPiece)

            else
                case Piece.square piece of
                    Nothing ->
                        Just (IllegalMoveError PieceNotOnBoard)

                    Just square ->
                        if moveGoesOffBoard squaresTraveled then
                            Just (IllegalMoveError MoveGoesOffBoard)

                        else if capture && (pieceAt to thePosition == Nothing) then
                            Just (IllegalMoveError CaptureMoveHasNoTarget)

                        else if not capture && (pieceAt to thePosition /= Nothing) then
                            Just (IllegalMoveError NonCaptureMoveLandsOnPiece)

                        else if Maybe.map Piece.player (pieceAt to thePosition) == Just positionDetails.playerToMove then
                            Just (IllegalMoveError MoveLandsOnPlayersOwnPiece)

                        else if
                            squaresContainPieces (List.filterMap identity squaresTraveled) thePosition
                                && not jumpsAllowed
                        then
                            Just (IllegalMoveError PiecesBlockMovePath)

                        else
                            Nothing


moveGoesOffBoard : List (Maybe Square.Square) -> Bool
moveGoesOffBoard maybeSquares =
    maybeSquares
        |> List.any Maybe.Extra.isNothing


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


type Error
    = IllegalMoveError IllegalMoveReason


type IllegalMoveReason
    = PieceNotOnBoard
    | MoveGoesOffBoard
    | PiecesBlockMovePath
    | NotCurrentPlayersPiece
    | CaptureMoveHasNoTarget
    | NonCaptureMoveLandsOnPiece
    | MoveLandsOnPlayersOwnPiece
    | NoPieceFoundAtSquare Square.Square
    | ViolatesMovementRulesOfPiece Piece.PieceType


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
