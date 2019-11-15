module Main exposing (main)

import Browser
import Html exposing (..)
import Html.Attributes as Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode as Decode
import Json.Encode as Encode
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
    , coordinateValue : Maybe Float
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { gamma = 2.2
      , coordinateValue = Nothing
      }
    , Cmd.none
    )



-- UPDATE


type Msg
    = OnImageClick ( Int, Int )
    | OnGammaChange Int
    | GotValueAtCoordinate (Result Http.Error Float)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        OnImageClick ( x, y ) ->
            ( model
            , Http.post
                { url =
                    Url.absolute [ "image", "coordinate" ]
                        [ Url.string "gamma" (String.fromFloat model.gamma) ]
                , expect = Http.expectJson GotValueAtCoordinate Decode.float
                , body = Http.jsonBody (Encode.list identity [ Encode.int x, Encode.int y ])
                }
            )

        OnGammaChange gamma ->
            ( { model | gamma = toFloat gamma / 100 }
            , Cmd.none
            )

        GotValueAtCoordinate value ->
            ( { model | coordinateValue = Result.toMaybe value }
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
                , on "click" <|
                    Decode.map4 (\x y tx ty -> OnImageClick ( x - tx, y - ty ))
                        (Decode.field "x" Decode.int)
                        (Decode.field "y" Decode.int)
                        (Decode.at [ "target", "x" ] Decode.int)
                        (Decode.at [ "target", "y" ] Decode.int)
                ]
                []
            , div []
                [ label [] [ text "pos" ]
                , p [] [ text (Debug.toString model.coordinateValue) ]
                ]
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
