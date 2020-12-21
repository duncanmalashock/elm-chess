module Main exposing (main)

import Browser
import Html
import Html.Attributes
import Html.Events
import Piece
import Player
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
    , boardPOV : Player.Player
    }


type Msg
    = SquareClicked Square.Square
    | ClickedFlipBoard


init : Flags -> ( Model, Cmd Msg )
init flags =
    let
        initialPosition =
            Position.init Position.defaultSetup
    in
    ( { position = initialPosition
      , selectedSquare = Nothing
      , boardPOV = Player.White
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
            , Html.Attributes.style "font-size" "48px"
            , Html.Attributes.style "font-family" "Courier New"
            ]
            [ viewBoard model
            ]
        , Html.div []
            [ Html.text (playerToString (Position.playerToMove model.position)) ]
        , Html.div []
            [ Html.text (errorToString (Position.lastError model.position)) ]
        , Html.button
            [ Html.Events.onClick ClickedFlipBoard
            ]
            [ Html.text "Flip board" ]
        ]
    }


playerToString : Player.Player -> String
playerToString player =
    case player of
        Player.White ->
            "White to move"

        Player.Black ->
            "Black to move"


viewBoard : Model -> Html.Html Msg
viewBoard model =
    Position.view
        { viewSquare = viewSquare model.selectedSquare
        , viewRank = viewRank
        , viewAllRanks = viewAllRanks
        }
        model.boardPOV
        model.position


viewSquare : Maybe Square.Square -> { square : Square.Square, piece : Maybe Piece.Piece } -> Html.Html Msg
viewSquare maybeSelectedSquare { square, piece } =
    Html.span
        [ Html.Attributes.style "background-color"
            (if maybeSelectedSquare == Just square then
                "green"

             else
                case Square.color square of
                    Square.White ->
                        "white"

                    Square.Black ->
                        "#d4d4d4"
            )
        , Html.Attributes.style "text-align" "center"
        , Html.Attributes.style "display" "inline-block"
        , Html.Attributes.style "width" "50px"
        , Html.Attributes.style "height" "50px"
        , Html.Events.onClick (SquareClicked square)
        ]
        [ Html.text
            (piece
                |> Maybe.map Piece.toSymbol
                |> Maybe.withDefault " "
            )
        ]


viewRank : List (Html.Html msg) -> Html.Html msg
viewRank squaresView =
    Html.div [] squaresView


viewAllRanks : List (Html.Html msg) -> Html.Html msg
viewAllRanks ranksView =
    Html.div [] ranksView


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
        SquareClicked clickedSquare ->
            case model.selectedSquare of
                Nothing ->
                    ( { model
                        | selectedSquare = Just clickedSquare
                      }
                    , Cmd.none
                    )

                Just selectedSquare ->
                    ( { model
                        | selectedSquare = Nothing
                        , position =
                            model.position
                                |> Position.play
                                    { from = selectedSquare
                                    , to = clickedSquare
                                    }
                      }
                    , Cmd.none
                    )

        ClickedFlipBoard ->
            ( { model
                | boardPOV = Player.opponent model.boardPOV
              }
            , Cmd.none
            )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
