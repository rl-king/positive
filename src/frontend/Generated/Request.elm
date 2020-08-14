module Generated.Request exposing (getImageList, getImageSettings, postImageSettings)

import Generated.Data.ImageSettings
import Http
import Json.Decode


postImageSettings :
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
postImageSettings a b =
    Http.request
        { method = "POST"
        , headers = []
        , url = "/image/settings?dir=" ++ a
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


getImageList :
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
                (List String)
            )
getImageList a =
    Http.request
        { method = "GET"
        , headers = []
        , url = "/image/list?dir=" ++ a
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
                                (Json.Decode.decodeString (Json.Decode.list Json.Decode.string) d)
                )
        , timeout = Nothing
        , tracker = Nothing
        }