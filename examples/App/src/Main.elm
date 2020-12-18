module Main exposing (main)

import Browser
import Html
import Html.Attributes
import Position
import Square


main : Program Flags Model Msg
main =
    Browser.document
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type alias Flags =
    {}


type alias Model =
    { position : Position.Position
    , selectedSquare : Maybe Square.Square
    }


type Msg
    = NoOp


init : Flags -> ( Model, Cmd Msg )
init flags =
    ( { position =
            Position.init Position.defaultSetup
                |> Position.play { from = Square.e2, to = Square.e4 }
                |> Position.play { from = Square.e7, to = Square.e5 }
                |> Position.play { from = Square.g1, to = Square.f3 }
                |> Position.play { from = Square.b8, to = Square.c6 }
                |> Position.play { from = Square.f1, to = Square.b5 }
      , selectedSquare = Nothing
      }
    , Cmd.none
    )


view : Model -> Browser.Document Msg
view model =
    { title = "Chess App"
    , body =
        [ Html.div
            [ Html.Attributes.style "white-space" "pre"
            , Html.Attributes.style "line-height" "1.0"
            , Html.Attributes.style "letter-spacing" "10px"
            , Html.Attributes.style "font-size" "48px"
            , Html.Attributes.style "font-family" "Courier New"
            ]
            [ Html.text (Position.toString model.position)
            ]
        , Html.text (errorToString (Position.lastError model.position))
        ]
    }


errorToString : Maybe Position.Error -> String
errorToString maybeError =
    case maybeError of
        Just error ->
            case error of
                Position.IllegalMoveError illegalMoveReason ->
                    case illegalMoveReason of
                        Position.PieceNotOnBoard ->
                            "Piece is not on the board."

                        Position.MoveGoesOffBoard ->
                            "That move would go off the board."

                        Position.PiecesBlockMovePath ->
                            "That piece cannot jump over other pieces."

                        Position.NotCurrentPlayersPiece ->
                            "That piece does not belong to the player to move."

                        Position.CaptureMoveHasNoTarget ->
                            "Piece attempted to capture, but there is no piece at the target square."

                        Position.NonCaptureMoveLandsOnPiece ->
                            "Piece cannot move onto the piece without capturing."

                        Position.MoveLandsOnPlayersOwnPiece ->
                            "That move would land on the player's own piece."

                        Position.NoPieceFoundAtSquare square ->
                            "There is no piece at that square to move."

                        Position.ViolatesMovementRulesOfPiece pieceType ->
                            "The selected piece cannot move that way."

        Nothing ->
            ""


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
