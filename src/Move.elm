module Move exposing
    ( Move(..)
    , movesForPiece
    , piece
    , toString
    )

import Piece
import Square


type Move
    = Move Piece.Piece { from : Square.Square, to : Square.Square } (List (Maybe Square.Square))


piece : Move -> Piece.Piece
piece move =
    case move of
        Move thePiece _ _ ->
            thePiece


toString : Move -> String
toString theMove =
    case theMove of
        Move thePiece { from, to } squaresTraveled ->
            Piece.toString thePiece
                ++ Square.toString from
                ++ "-"
                ++ Square.toString to


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
