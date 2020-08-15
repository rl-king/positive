module Generated.Request exposing (getImageSettings, postImageSettings)

import Generated.Data.ImageSettings
import Http
import Json.Decode
import Json.Encode


postImageSettings :
    String
    -> List Generated.Data.ImageSettings.ImageSettings
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
postImageSettings a b =
    Http.request
        { method = "POST"
        , headers = []
        , url = "/image/settings?dir=" ++ a
        , body = Http.jsonBody (Json.Encode.list Generated.Data.ImageSettings.encodeImageSettings b)
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
                                (Json.Decode.decodeString (Json.Decode.list Generated.Data.ImageSettings.decodeImageSettings) e)
                )
        , timeout = Nothing
        , tracker = Nothing
        }


getImageSettings :
    String
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
getImageSettings a =
    Http.request
        { method = "GET"
        , headers = []
        , url = "/image/settings?dir=" ++ a
        , body = Http.emptyBody
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
