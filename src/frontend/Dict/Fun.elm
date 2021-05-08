module Dict.Fun exposing
    ( Dict
    , empty
    , get
    , update
    )

import Dict


type Dict k comparable v
    = Dict (k -> comparable) (comparable -> k) (Dict.Dict comparable v)


empty : (k -> comparable) -> (comparable -> k) -> Dict k comparable v
empty f g =
    Dict f g Dict.empty


get : k -> Dict k comparable v -> Maybe v
get k (Dict f _ dict) =
    Dict.get (f k) dict


update : k -> (Maybe v -> Maybe v) -> Dict k comparable v -> Dict k comparable v
update k h (Dict f g dict) =
    Dict f g <|
        Dict.update (f k) h dict
