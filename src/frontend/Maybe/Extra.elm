module Maybe.Extra exposing (unwrap)


unwrap : b -> (a -> b) -> Maybe a -> b
unwrap default f =
    Maybe.withDefault default << Maybe.map f
