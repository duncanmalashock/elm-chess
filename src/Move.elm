module Move exposing
    ( Move(..)
    , from
    , movesForPiece
    , piece
    , to
    , toString
    )

import Piece
import Square


type Move
    = Move Piece.Piece { from : Square.Square, to : Square.Square, capture : Bool, jumpsAllowed : Bool } (List (Maybe Square.Square))


piece : Move -> Piece.Piece
piece move =
    case move of
        Move thePiece _ _ ->
            thePiece


to : Move -> Square.Square
to (Move _ moveDetails _) =
    moveDetails.to


from : Move -> Square.Square
from (Move _ moveDetails _) =
    moveDetails.from


toString : Move -> String
toString theMove =
    case theMove of
        Move thePiece moveDetails squaresTraveled ->
            Piece.toString thePiece
                ++ Square.toString moveDetails.from
                ++ "-"
                ++ Square.toString moveDetails.to


movesForPiece : Piece.Piece -> List Move
movesForPiece thePiece =
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
            moveFromMovementRuleHelp thePiece startingSquare { capture = False, stoppedByInterveningPieces = stoppedByInterveningPieces } steps

        Piece.CaptureRule { stoppedByInterveningPieces } steps ->
            moveFromMovementRuleHelp thePiece startingSquare { capture = True, stoppedByInterveningPieces = stoppedByInterveningPieces } steps


moveFromMovementRuleHelp : Piece.Piece -> Square.Square -> { capture : Bool, stoppedByInterveningPieces : Bool } -> List Square.Step -> Maybe Move
moveFromMovementRuleHelp thePiece startingSquare { capture, stoppedByInterveningPieces } steps =
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
                    , capture = capture
                    , jumpsAllowed = not stoppedByInterveningPieces
                    }
                    (squaresFromSteps steps startingSquare)
            )


squaresFromSteps : List Square.Step -> Square.Square -> List (Maybe Square.Square)
squaresFromSteps steps startingSquare =
    squaresFromStepsHelp steps startingSquare []


squaresFromStepsHelp : List Square.Step -> Square.Square -> List (Maybe Square.Square) -> List (Maybe Square.Square)
squaresFromStepsHelp steps startingSquare outputSquaresSoFar =
    case steps of
        [] ->
            outputSquaresSoFar

        head :: tail ->
            let
                newStartingSquare : Maybe Square.Square
                newStartingSquare =
                    Square.applyStep head startingSquare

                newOutputSquaresSoFar : List (Maybe Square.Square)
                newOutputSquaresSoFar =
                    [ newStartingSquare ] ++ outputSquaresSoFar
            in
            squaresFromStepsHelp
                tail
                (newStartingSquare |> Maybe.withDefault startingSquare)
                newOutputSquaresSoFar
