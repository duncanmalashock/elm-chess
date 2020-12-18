module Main exposing (main)

import Browser
import Html
import Position


main : Program Flags Model Msg
main =
    Browser.document
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


init : Flags -> ( Model, Cmd Msg )
init flags =
    ( {}, Cmd.none )


view : Model -> Browser.Document Msg
view model =
    { title = "Chess App"
    , body =
        [ Html.text "Hello, world!"
        ]
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


type alias Flags =
    {}


type alias Model =
    {}


type Msg
    = NoOp
