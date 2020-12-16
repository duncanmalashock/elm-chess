module Test.Position exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Piece
import Player
import Position
import Square
import Test exposing (..)


suite : Test
suite =
    describe "a"
        [ test "aa" <|
            \_ ->
                let
                    expected =
                        ""

                    result =
                        let
                            pawnE4 =
                                Position.move
                                    (Piece.pawn Player.White Square.e2)
                                    Square.e4

                            pawnE5 =
                                Position.move
                                    (Piece.pawn Player.Black Square.e7)
                                    Square.e5

                            setup =
                                Just (Position.init Position.defaultSetup)
                        in
                        setup
                            |> Maybe.map2 (\theMove -> Position.tryMove theMove) pawnE4
                            |> Maybe.map2 (\theMove -> Position.tryMove theMove) pawnE5
                            |> Maybe.map Position.toString
                            |> Maybe.withDefault ""
                            |> Debug.log "      "
                in
                Expect.equal expected result
        ]
