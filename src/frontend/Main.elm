module Main exposing (main)

import Browser
import Html exposing (..)
import Html.Attributes as Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode as Decode
import Url.Builder as Url



-- MAIN


main : Program () Model Msg
main =
    Browser.document
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }



-- MODEL


type alias Model =
    { gamma : Float
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { gamma = 2.2
      }
    , Cmd.none
    )



-- UPDATE


type Msg
    = OnGammaChange Int


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        OnGammaChange gamma ->
            ( { model | gamma = toFloat gamma / 100 }
            , Cmd.none
            )



-- VIEW


view : Model -> Browser.Document Msg
view model =
    { title = "Positive"
    , body =
        [ main_ []
            [ img
                [ src <|
                    Url.absolute
                        [ "image" ]
                        [ Url.string "gamma" (String.fromFloat model.gamma) ]

                -- , on "click" (Decode.dd
                ]
                []
            , div []
                [ label [] [ text "gamma" ]
                , p [] [ text (String.fromFloat model.gamma) ]
                , input
                    [ type_ "range"
                    , value (String.fromInt (floor (model.gamma * 100)))
                    , Attributes.min "0"
                    , Attributes.max "800"
                    , on "change" <|
                        Decode.map OnGammaChange
                            (Decode.at [ "target", "valueAsNumber" ] Decode.int)
                    , style "width" "16rem"
                    ]
                    []
                ]
            ]
        ]
    }
