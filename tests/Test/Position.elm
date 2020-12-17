module Test.Position exposing (..)

import Expect exposing (Expectation)
import Move
import Piece
import Player
import Position
import Square
import Test exposing (..)


suite : Test
suite =
    describe "Position" <|
        [ describe "legalMoves"
            [ test "There should be 20 legal moves from the initial position" <|
                \_ ->
                    let
                        expected =
                            20

                        result =
                            Position.init Position.defaultSetup
                                |> Position.legalMoves { ignoreTurn = False }
                                |> List.length
                    in
                    Expect.equal expected result
            ]
        , describe "play"
            [ test "Can play a legal opening move" <|
                \_ ->
                    let
                        expected =
                            Nothing

                        result =
                            Position.init Position.defaultSetup
                                |> Position.play { from = Square.e2, to = Square.e4 }
                                |> Position.lastError
                    in
                    Expect.equal expected result
            , test "Can't move a piece in the wrong way" <|
                \_ ->
                    let
                        expected =
                            Just <|
                                Position.IllegalMoveError <|
                                    Position.ViolatesMovementRulesOfPiece Piece.Pawn

                        result =
                            Position.init Position.defaultSetup
                                |> Position.play { from = Square.e2, to = Square.e5 }
                                |> Position.lastError
                    in
                    Expect.equal expected result
            , test "Can't move a non-jumping piece over other pieces" <|
                \_ ->
                    let
                        expected =
                            Just <|
                                Position.IllegalMoveError <|
                                    Position.PiecesBlockMovePath

                        result =
                            Position.init Position.defaultSetup
                                |> Position.play { from = Square.h1, to = Square.h3 }
                                |> Position.lastError
                    in
                    Expect.equal expected result
            , test "Can't make a capture move with nothing there" <|
                \_ ->
                    let
                        expected =
                            Just <|
                                Position.IllegalMoveError <|
                                    Position.ViolatesMovementRulesOfPiece Piece.Pawn

                        result =
                            Position.init Position.defaultSetup
                                |> Position.play { from = Square.f2, to = Square.g3 }
                                |> Position.lastError
                    in
                    Expect.equal expected result
            , test "Can't make a non-capture move into a piece" <|
                \_ ->
                    let
                        expected =
                            Just <|
                                Position.IllegalMoveError <|
                                    Position.ViolatesMovementRulesOfPiece Piece.Pawn

                        result =
                            Position.init Position.defaultSetup
                                |> Position.play { from = Square.e2, to = Square.e4 }
                                |> Position.play { from = Square.e7, to = Square.e5 }
                                |> Position.play { from = Square.e4, to = Square.e5 }
                                |> Position.lastError
                    in
                    Expect.equal expected result
            , test "Can play multiple moves" <|
                \_ ->
                    let
                        expected =
                            Nothing

                        result =
                            Position.init Position.defaultSetup
                                |> Position.play { from = Square.e2, to = Square.e4 }
                                |> Position.play { from = Square.e7, to = Square.e5 }
                                |> Position.play { from = Square.g1, to = Square.f3 }
                                |> Position.play { from = Square.b8, to = Square.c6 }
                                |> Position.play { from = Square.f1, to = Square.b5 }
                                |> Position.lastError
                    in
                    Expect.equal expected result
            , test "Can't play twice in a row for the same player" <|
                \_ ->
                    let
                        expected =
                            Just <|
                                Position.IllegalMoveError
                                    Position.NotCurrentPlayersPiece

                        result =
                            Position.init Position.defaultSetup
                                |> Position.play { from = Square.e2, to = Square.e4 }
                                |> Position.play { from = Square.d2, to = Square.d4 }
                                |> Position.lastError
                    in
                    Expect.equal expected result
            ]
        , describe "attackers"
            [ test "e5 should be attacked by f3 in move 2 of the Ruy Lopez" <|
                \_ ->
                    let
                        expected =
                            [ Piece.knight Player.White Square.f3
                            ]

                        result =
                            Position.init Position.defaultSetup
                                |> Position.play { from = Square.e2, to = Square.e4 }
                                |> Position.play { from = Square.e7, to = Square.e5 }
                                |> Position.play { from = Square.g1, to = Square.f3 }
                                |> Position.attackers Player.Black Square.e5
                    in
                    Expect.equal expected result
            ]
        , describe "defenders"
            [ test "e5 should be defended by c6 in move 2 of the Ruy Lopez" <|
                \_ ->
                    let
                        expected =
                            []

                        result =
                            Position.init Position.defaultSetup
                                |> Position.play { from = Square.e2, to = Square.e4 }
                                |> Position.play { from = Square.e7, to = Square.e5 }
                                |> Position.play { from = Square.g1, to = Square.f3 }
                                |> Position.play { from = Square.b7, to = Square.c6 }
                                |> Position.defenders Player.Black Square.e5
                    in
                    Expect.equal expected result
            ]
        ]
