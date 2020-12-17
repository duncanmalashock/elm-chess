module Square exposing
    ( Square
    , a1, a2, a3, a4, a5, a6, a7, a8
    , b1, b2, b3, b4, b5, b6, b7, b8
    , c1, c2, c3, c4, c5, c6, c7, c8
    , d1, d2, d3, d4, d5, d6, d7, d8
    , e1, e2, e3, e4, e5, e6, e7, e8
    , f1, f2, f3, f4, f5, f6, f7, f8
    , g1, g2, g3, g4, g5, g6, g7, g8
    , h1, h2, h3, h4, h5, h6, h7, h8
    , square
    , a, b, c, d, e, f, g, h
    , one, two, three, four, five, six, seven, eight
    , Step
    , north, south, east, west
    , northeast, northwest, southeast, southwest
    , allFiles, allOnFile, allOnRank, allRanks, applyStep, toString
    )

{-|

@docs Square
@docs a1, a2, a3, a4, a5, a6, a7, a8
@docs b1, b2, b3, b4, b5, b6, b7, b8
@docs c1, c2, c3, c4, c5, c6, c7, c8
@docs d1, d2, d3, d4, d5, d6, d7, d8
@docs e1, e2, e3, e4, e5, e6, e7, e8
@docs f1, f2, f3, f4, f5, f6, f7, f8
@docs g1, g2, g3, g4, g5, g6, g7, g8
@docs h1, h2, h3, h4, h5, h6, h7, h8
@docs square

@docs a, b, c, d, e, f, g, h
@docs one, two, three, four, five, six, seven, eight

@docs Step
@docs north, south, east, west
@docs northeast, northwest, southeast, southwest

-}


type Step
    = North
    | South
    | East
    | West
    | Northwest
    | Northeast
    | Southwest
    | Southeast


allFiles : List File
allFiles =
    [ a, b, c, d, e, f, g, h ]


allRanks : List Rank
allRanks =
    [ one
    , two
    , three
    , four
    , five
    , six
    , seven
    , eight
    ]


allOnFile : File -> List Square
allOnFile file =
    List.map (\rank -> square file rank) allRanks


allOnRank : Rank -> List Square
allOnRank rank =
    List.map (\file -> square file rank) allFiles


toString : Square -> String
toString (Square file rank) =
    fileToString file ++ rankToString rank


rankToString : Rank -> String
rankToString rank =
    case rank of
        Rank1 ->
            "1"

        Rank2 ->
            "2"

        Rank3 ->
            "3"

        Rank4 ->
            "4"

        Rank5 ->
            "5"

        Rank6 ->
            "6"

        Rank7 ->
            "7"

        Rank8 ->
            "8"


fileToString : File -> String
fileToString file =
    case file of
        FileA ->
            "a"

        FileB ->
            "b"

        FileC ->
            "c"

        FileD ->
            "d"

        FileE ->
            "e"

        FileF ->
            "f"

        FileG ->
            "g"

        FileH ->
            "h"


applyStep : Step -> Square -> Maybe Square
applyStep step startingSquare =
    case startingSquare of
        Square file rank ->
            case step of
                North ->
                    Maybe.map
                        (\newRank ->
                            Square file newRank
                        )
                        (nextRank rank)

                South ->
                    Maybe.map
                        (\newRank ->
                            Square file newRank
                        )
                        (prevRank rank)

                East ->
                    Maybe.map
                        (\newFile ->
                            Square newFile rank
                        )
                        (nextFile file)

                West ->
                    Maybe.map
                        (\newFile ->
                            Square newFile rank
                        )
                        (prevFile file)

                Northwest ->
                    Maybe.map
                        (\newRank ->
                            Square file newRank
                        )
                        (nextRank rank)
                        |> Maybe.map2
                            (\newFile maybeNewSquare ->
                                case maybeNewSquare of
                                    Square _ r ->
                                        Square newFile r
                            )
                            (prevFile file)

                Northeast ->
                    Maybe.map
                        (\newRank ->
                            Square file newRank
                        )
                        (nextRank rank)
                        |> Maybe.map2
                            (\newFile maybeNewSquare ->
                                case maybeNewSquare of
                                    Square _ r ->
                                        Square newFile r
                            )
                            (nextFile file)

                Southwest ->
                    Maybe.map
                        (\newRank ->
                            Square file newRank
                        )
                        (prevRank rank)
                        |> Maybe.map2
                            (\newFile maybeNewSquare ->
                                case maybeNewSquare of
                                    Square _ r ->
                                        Square newFile r
                            )
                            (prevFile file)

                Southeast ->
                    Maybe.map
                        (\newRank ->
                            Square file newRank
                        )
                        (prevRank rank)
                        |> Maybe.map2
                            (\newFile maybeNewSquare ->
                                case maybeNewSquare of
                                    Square _ r ->
                                        Square newFile r
                            )
                            (nextFile file)


nextFile : File -> Maybe File
nextFile currentFile =
    case currentFile of
        FileA ->
            Just FileB

        FileB ->
            Just FileC

        FileC ->
            Just FileD

        FileD ->
            Just FileE

        FileE ->
            Just FileF

        FileF ->
            Just FileG

        FileG ->
            Just FileH

        FileH ->
            Nothing


prevFile : File -> Maybe File
prevFile currentFile =
    case currentFile of
        FileA ->
            Nothing

        FileB ->
            Just FileA

        FileC ->
            Just FileB

        FileD ->
            Just FileC

        FileE ->
            Just FileD

        FileF ->
            Just FileE

        FileG ->
            Just FileF

        FileH ->
            Just FileG


nextRank : Rank -> Maybe Rank
nextRank currentRank =
    case currentRank of
        Rank1 ->
            Just Rank2

        Rank2 ->
            Just Rank3

        Rank3 ->
            Just Rank4

        Rank4 ->
            Just Rank5

        Rank5 ->
            Just Rank6

        Rank6 ->
            Just Rank7

        Rank7 ->
            Just Rank8

        Rank8 ->
            Nothing


prevRank : Rank -> Maybe Rank
prevRank currentRank =
    case currentRank of
        Rank1 ->
            Nothing

        Rank2 ->
            Just Rank1

        Rank3 ->
            Just Rank2

        Rank4 ->
            Just Rank3

        Rank5 ->
            Just Rank4

        Rank6 ->
            Just Rank5

        Rank7 ->
            Just Rank6

        Rank8 ->
            Just Rank7


north : Step
north =
    North


south : Step
south =
    South


east : Step
east =
    East


west : Step
west =
    West


northwest : Step
northwest =
    Northwest


northeast : Step
northeast =
    Northeast


southwest : Step
southwest =
    Southwest


southeast : Step
southeast =
    Southeast


a : File
a =
    FileA


b : File
b =
    FileB


c : File
c =
    FileC


d : File
d =
    FileD


e : File
e =
    FileE


f : File
f =
    FileF


g : File
g =
    FileG


h : File
h =
    FileH


one : Rank
one =
    Rank1


two : Rank
two =
    Rank2


three : Rank
three =
    Rank3


four : Rank
four =
    Rank4


five : Rank
five =
    Rank5


six : Rank
six =
    Rank6


seven : Rank
seven =
    Rank7


eight : Rank
eight =
    Rank8


type Square
    = Square File Rank


square : File -> Rank -> Square
square file rank =
    Square file rank


a1 : Square
a1 =
    Square FileA Rank1


a2 : Square
a2 =
    Square FileA Rank2


a3 : Square
a3 =
    Square FileA Rank3


a4 : Square
a4 =
    Square FileA Rank4


a5 : Square
a5 =
    Square FileA Rank5


a6 : Square
a6 =
    Square FileA Rank6


a7 : Square
a7 =
    Square FileA Rank7


a8 : Square
a8 =
    Square FileA Rank8


b1 : Square
b1 =
    Square FileB Rank1


b2 : Square
b2 =
    Square FileB Rank2


b3 : Square
b3 =
    Square FileB Rank3


b4 : Square
b4 =
    Square FileB Rank4


b5 : Square
b5 =
    Square FileB Rank5


b6 : Square
b6 =
    Square FileB Rank6


b7 : Square
b7 =
    Square FileB Rank7


b8 : Square
b8 =
    Square FileB Rank8


c1 : Square
c1 =
    Square FileC Rank1


c2 : Square
c2 =
    Square FileC Rank2


c3 : Square
c3 =
    Square FileC Rank3


c4 : Square
c4 =
    Square FileC Rank4


c5 : Square
c5 =
    Square FileC Rank5


c6 : Square
c6 =
    Square FileC Rank6


c7 : Square
c7 =
    Square FileC Rank7


c8 : Square
c8 =
    Square FileC Rank8


d1 : Square
d1 =
    Square FileD Rank1


d2 : Square
d2 =
    Square FileD Rank2


d3 : Square
d3 =
    Square FileD Rank3


d4 : Square
d4 =
    Square FileD Rank4


d5 : Square
d5 =
    Square FileD Rank5


d6 : Square
d6 =
    Square FileD Rank6


d7 : Square
d7 =
    Square FileD Rank7


d8 : Square
d8 =
    Square FileD Rank8


e1 : Square
e1 =
    Square FileE Rank1


e2 : Square
e2 =
    Square FileE Rank2


e3 : Square
e3 =
    Square FileE Rank3


e4 : Square
e4 =
    Square FileE Rank4


e5 : Square
e5 =
    Square FileE Rank5


e6 : Square
e6 =
    Square FileE Rank6


e7 : Square
e7 =
    Square FileE Rank7


e8 : Square
e8 =
    Square FileE Rank8


f1 : Square
f1 =
    Square FileF Rank1


f2 : Square
f2 =
    Square FileF Rank2


f3 : Square
f3 =
    Square FileF Rank3


f4 : Square
f4 =
    Square FileF Rank4


f5 : Square
f5 =
    Square FileF Rank5


f6 : Square
f6 =
    Square FileF Rank6


f7 : Square
f7 =
    Square FileF Rank7


f8 : Square
f8 =
    Square FileF Rank8


g1 : Square
g1 =
    Square FileG Rank1


g2 : Square
g2 =
    Square FileG Rank2


g3 : Square
g3 =
    Square FileG Rank3


g4 : Square
g4 =
    Square FileG Rank4


g5 : Square
g5 =
    Square FileG Rank5


g6 : Square
g6 =
    Square FileG Rank6


g7 : Square
g7 =
    Square FileG Rank7


g8 : Square
g8 =
    Square FileG Rank8


h1 : Square
h1 =
    Square FileH Rank1


h2 : Square
h2 =
    Square FileH Rank2


h3 : Square
h3 =
    Square FileH Rank3


h4 : Square
h4 =
    Square FileH Rank4


h5 : Square
h5 =
    Square FileH Rank5


h6 : Square
h6 =
    Square FileH Rank6


h7 : Square
h7 =
    Square FileH Rank7


h8 : Square
h8 =
    Square FileH Rank8


type Rank
    = Rank1
    | Rank2
    | Rank3
    | Rank4
    | Rank5
    | Rank6
    | Rank7
    | Rank8


type File
    = FileA
    | FileB
    | FileC
    | FileD
    | FileE
    | FileF
    | FileG
    | FileH
