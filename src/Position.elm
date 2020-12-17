module Position exposing
    ( defaultSetup
    , init
    , legalMoves
    , toString
    , tryMove
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


legalMoves : Position -> List Move.Move
legalMoves ((Position details) as thePosition) =
    List.concatMap Move.movesForPiece details.pieces
        |> List.filter (moveIsLegal thePosition)


moveIsLegal : Position -> Move.Move -> Bool
moveIsLegal thePosition theMove =
    validateMove thePosition theMove
        |> (\( _, maybeError ) ->
                maybeError == Nothing
           )


validateMove : Position -> Move.Move -> ( Position, Maybe Error )
validateMove ((Position positionDetails) as thePosition) theMove =
    case theMove of
        Move.Move piece { from, to, capture, jumpsAllowed } squaresTraveled ->
            if Piece.player piece /= positionDetails.playerToMove then
                ( thePosition, Just (IllegalMoveError theMove NotCurrentPlayersPiece) )

            else
                case Piece.square piece of
                    Nothing ->
                        ( thePosition, Just (IllegalMoveError theMove PieceNotOnBoard) )

                    Just square ->
                        if
                            squaresContainPieces (List.filterMap identity squaresTraveled) thePosition
                                && not jumpsAllowed
                        then
                            ( thePosition, Just (IllegalMoveError theMove PiecesBlockMovePath) )

                        else if moveGoesOffBoard squaresTraveled then
                            ( thePosition, Just (IllegalMoveError theMove MoveGoesOffBoard) )

                        else if capture && (pieceAt to thePosition == Nothing) then
                            ( thePosition, Just (IllegalMoveError theMove CaptureMoveHasNoTarget) )

                        else if not capture && (pieceAt to thePosition /= Nothing) then
                            ( thePosition, Just (IllegalMoveError theMove NonCaptureMoveLandsOnPiece) )

                        else if Maybe.map Piece.player (pieceAt to thePosition) == Just positionDetails.playerToMove then
                            ( thePosition, Just (IllegalMoveError theMove MoveLandsOnPlayersOwnPiece) )

                        else
                            ( thePosition, Nothing )


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
    = IllegalMoveError Move.Move IllegalMoveReason


type IllegalMoveReason
    = PieceNotOnBoard
    | MoveGoesOffBoard
    | PiecesBlockMovePath
    | NotCurrentPlayersPiece
    | CaptureMoveHasNoTarget
    | NonCaptureMoveLandsOnPiece
    | MoveLandsOnPlayersOwnPiece


tryMove : Move.Move -> Position -> Position
tryMove theMove (Position positionDetails) =
    case theMove of
        Move.Move piece { from, to } squaresTraveled ->
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
