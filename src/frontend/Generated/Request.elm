module Generated.Request exposing (getImageSettings, postImageSettings, postImageSettingsHighres, postImageSettingsHistogram, postImageSettingsPreviews)

import Generated.Data.ImageSettings
import Http
import Json.Decode
import Json.Encode


postImageSettings :
    List Generated.Data.ImageSettings.ImageSettings
    ->
        Cmd
            (Result
                ( Http.Error
                , Maybe
                    { metadata : Http.Metadata
                    , body : String
                    }
                )
                (List Generated.Data.ImageSettings.ImageSettings)
            )
postImageSettings a =
    Http.request
        { method = "POST"
        , headers = []
        , url = "/image/settings"
        , body = Http.jsonBody (Json.Encode.list Generated.Data.ImageSettings.encodeImageSettings a)
        , expect =
            Http.expectStringResponse identity
                (\b ->
                    case b of
                        Http.BadUrl_ c ->
                            Err ( Http.BadUrl c, Nothing )

                        Http.Timeout_ ->
                            Err ( Http.Timeout, Nothing )

                        Http.NetworkError_ ->
                            Err ( Http.NetworkError, Nothing )

                        Http.BadStatus_ c d ->
                            Err ( Http.BadStatus c.statusCode, Just { metadata = c, body = d } )

                        Http.GoodStatus_ c d ->
                            Result.mapError
                                (\e ->
                                    ( Http.BadBody (Json.Decode.errorToString e)
                                    , Just
                                        { metadata = c
                                        , body = d
                                        }
                                    )
                                )
                                (Json.Decode.decodeString (Json.Decode.list Generated.Data.ImageSettings.decodeImageSettings) d)
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
            ( List Generated.Data.ImageSettings.ImageSettings, Generated.Data.ImageSettings.Dir )
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
                                (Json.Decode.decodeString (Json.Decode.map2 Tuple.pair (Json.Decode.index 0 (Json.Decode.list Generated.Data.ImageSettings.decodeImageSettings)) (Json.Decode.index 1 Generated.Data.ImageSettings.decodeDir)) c)
                )
        , timeout = Nothing
        , tracker = Nothing
        }


postImageSettingsHistogram :
    Int
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
postImageSettingsHistogram a b =
    Http.request
        { method = "POST"
        , headers = []
        , url = "/image/settings/histogram?preview-width=" ++ String.fromInt a
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
                            Result.mapError
                                (\f ->
                                    ( Http.BadBody (Json.Decode.errorToString f)
                                    , Just
                                        { metadata = d
                                        , body = e
                                        }
                                    )
                                )
                                (Json.Decode.decodeString (Json.Decode.list Json.Decode.int) e)
                )
        , timeout = Nothing
        , tracker = Nothing
        }


postImageSettingsHighres :
    Generated.Data.ImageSettings.ImageSettings
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
postImageSettingsHighres a =
    Http.request
        { method = "POST"
        , headers = []
        , url = "/image/settings/highres"
        , body = Http.jsonBody (Generated.Data.ImageSettings.encodeImageSettings a)
        , expect =
            Http.expectStringResponse identity
                (\b ->
                    case b of
                        Http.BadUrl_ c ->
                            Err ( Http.BadUrl c, Nothing )

                        Http.Timeout_ ->
                            Err ( Http.Timeout, Nothing )

                        Http.NetworkError_ ->
                            Err ( Http.NetworkError, Nothing )

                        Http.BadStatus_ c d ->
                            Err ( Http.BadStatus c.statusCode, Just { metadata = c, body = d } )

                        Http.GoodStatus_ c d ->
                            if d == "" then
                                Ok ()

                            else
                                Err
                                    ( Http.BadBody "Expected the response body to be empty"
                                    , Just
                                        { metadata = c
                                        , body = d
                                        }
                                    )
                )
        , timeout = Nothing
        , tracker = Nothing
        }


postImageSettingsPreviews :
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
postImageSettingsPreviews =
    Http.request
        { method = "POST"
        , headers = []
        , url = "/image/settings/previews"
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
                            if c == "" then
                                Ok ()

                            else
                                Err
                                    ( Http.BadBody "Expected the response body to be empty"
                                    , Just
                                        { metadata = b
                                        , body = c
                                        }
                                    )
                )
        , timeout = Nothing
        , tracker = Nothing
        }
