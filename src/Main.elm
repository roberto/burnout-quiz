module Main exposing (..)

import Browser
import Element exposing (layout, text)
import Html exposing (Html)


type alias Model =
    {}


type Msg
    = ClickedAnwser


main : Program () Model Msg
main =
    Browser.sandbox
        { init =
            {}
        , update = update
        , view = view
        }


update : Msg -> Model -> Model
update _ model =
    model


view : Model -> Html msg
view _ =
    layout
        []
    <|
        text
            "Melting..."
