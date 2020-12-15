module Example exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Position
import Test exposing (..)


suite : Test
suite =
    skip <|
        describe ""
            [ test "" <|
                \_ ->
                    let
                        expected =
                            1

                        result =
                            2
                    in
                    Expect.equal expected result
            ]
