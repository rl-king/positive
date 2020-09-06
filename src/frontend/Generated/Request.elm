module Generated.Request exposing (getImageSettings, postImageSettings, postImageSettingsHighres, postImageSettingsHistogram)

import Generated.Data.ImageSettings
import Http
import Json.Decode


postImageSettings :
    String
    -> Generated.Data.ImageSettings.FilmRollSettings
    ->
        Cmd
            (Result
                ( Http.Error
                , Maybe
                    { metadata : Http.Metadata
                    , body : String
                    }
                )
                Generated.Data.ImageSettings.FilmRollSettings
            )
postImageSettings a b =
    Http.request
        { method = "POST"
        , headers = []
        , url = "/image/settings?dir=" ++ a
        , body = Http.jsonBody (Generated.Data.ImageSettings.encodeFilmRollSettings b)
        , expect =
            Http.expectStringResponse identity
                (\c ->
                    case c of
                        Http.BadUrl_ d ->
                            Err ( Http.BadUrl d, Nothing )

                        Http.Timeout_ ->
                            Err ( Http.Timeout, Nothing )

                        Http.NetworkError_ ->
                            Err ( Http.NetworkError, Nothing )

                        Http.BadStatus_ d e ->
                            Err ( Http.BadStatus d.statusCode, Just { metadata = d, body = e } )

                        Http.GoodStatus_ d e ->
                            Result.mapError
                                (\f ->
                                    ( Http.BadBody (Json.Decode.errorToString f)
                                    , Just
                                        { metadata = d
                                        , body = e
                                        }
                                    )
                                )
                                (Json.Decode.decodeString Generated.Data.ImageSettings.decodeFilmRollSettings e)
                )
        , timeout = Nothing
        , tracker = Nothing
        }


getImageSettings :
    Cmd
        (Result
            ( Http.Error
            , Maybe
                { metadata : Http.Metadata
                , body : String
                }
            )
            (List ( String, Generated.Data.ImageSettings.FilmRollSettings ))
        )
getImageSettings =
    Http.request
        { method = "GET"
        , headers = []
        , url = "/image/settings"
        , body = Http.emptyBody
        , expect =
            Http.expectStringResponse identity
                (\a ->
                    case a of
                        Http.BadUrl_ b ->
                            Err ( Http.BadUrl b, Nothing )

                        Http.Timeout_ ->
                            Err ( Http.Timeout, Nothing )

                        Http.NetworkError_ ->
                            Err ( Http.NetworkError, Nothing )

                        Http.BadStatus_ b c ->
                            Err ( Http.BadStatus b.statusCode, Just { metadata = b, body = c } )

                        Http.GoodStatus_ b c ->
                            Result.mapError
                                (\d ->
                                    ( Http.BadBody (Json.Decode.errorToString d)
                                    , Just
                                        { metadata = b
                                        , body = c
                                        }
                                    )
                                )
                                (Json.Decode.decodeString (Json.Decode.list (Json.Decode.map2 Tuple.pair (Json.Decode.index 0 Json.Decode.string) (Json.Decode.index 1 Generated.Data.ImageSettings.decodeFilmRollSettings))) c)
                )
        , timeout = Nothing
        , tracker = Nothing
        }


postImageSettingsHistogram :
    String
    -> Int
    -> Generated.Data.ImageSettings.ImageSettings
    ->
        Cmd
            (Result
                ( Http.Error
                , Maybe
                    { metadata : Http.Metadata
                    , body : String
                    }
                )
                (List Int)
            )
postImageSettingsHistogram a b c =
    Http.request
        { method = "POST"
        , headers = []
        , url =
            String.concat
                [ "/image/settings/histogram?dir="
                , a
                , "&preview-width="
                , String.fromInt b
                ]
        , body = Http.jsonBody (Generated.Data.ImageSettings.encodeImageSettings c)
        , expect =
            Http.expectStringResponse identity
                (\d ->
                    case d of
                        Http.BadUrl_ e ->
                            Err ( Http.BadUrl e, Nothing )

                        Http.Timeout_ ->
                            Err ( Http.Timeout, Nothing )

                        Http.NetworkError_ ->
                            Err ( Http.NetworkError, Nothing )

                        Http.BadStatus_ e f ->
                            Err ( Http.BadStatus e.statusCode, Just { metadata = e, body = f } )

                        Http.GoodStatus_ e f ->
                            Result.mapError
                                (\g ->
                                    ( Http.BadBody (Json.Decode.errorToString g)
                                    , Just
                                        { metadata = e
                                        , body = f
                                        }
                                    )
                                )
                                (Json.Decode.decodeString (Json.Decode.list Json.Decode.int) f)
                )
        , timeout = Nothing
        , tracker = Nothing
        }


postImageSettingsHighres :
    String
    -> Generated.Data.ImageSettings.ImageSettings
    ->
        Cmd
            (Result
                ( Http.Error
                , Maybe
                    { metadata : Http.Metadata
                    , body : String
                    }
                )
                ()
            )
postImageSettingsHighres a b =
    Http.request
        { method = "POST"
        , headers = []
        , url = "/image/settings/highres?dir=" ++ a
        , body = Http.jsonBody (Generated.Data.ImageSettings.encodeImageSettings b)
        , expect =
            Http.expectStringResponse identity
                (\c ->
                    case c of
                        Http.BadUrl_ d ->
                            Err ( Http.BadUrl d, Nothing )

                        Http.Timeout_ ->
                            Err ( Http.Timeout, Nothing )

                        Http.NetworkError_ ->
                            Err ( Http.NetworkError, Nothing )

                        Http.BadStatus_ d e ->
                            Err ( Http.BadStatus d.statusCode, Just { metadata = d, body = e } )

                        Http.GoodStatus_ d e ->
                            if e == "" then
                                Ok ()

                            else
                                Err
                                    ( Http.BadBody "Expected the response body to be empty"
                                    , Just
                                        { metadata = d
                                        , body = e
                                        }
                                    )
                )
        , timeout = Nothing
        , tracker = Nothing
        }
