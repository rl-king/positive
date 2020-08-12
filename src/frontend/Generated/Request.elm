module Generated.Request exposing (getImageSettings)

import Generated.Data.ImageSettings
import Http
import Json.Decode


getImageSettings :
    Cmd
        (Result
            ( Http.Error
            , Maybe
                { metadata : Http.Metadata
                , body : String
                }
            )
            Generated.Data.ImageSettings.ImageSettings
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
                                (Json.Decode.decodeString Generated.Data.ImageSettings.decodeCoordinate c)
                )
        , timeout = Nothing
        , tracker = Nothing
        }
