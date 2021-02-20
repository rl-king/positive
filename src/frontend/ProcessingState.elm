module ProcessingState exposing
    ( ProcessingState(..)
    , State
    , map
    , preview
    , toData
    , toProcessing
    , toQueued
    , toReady
    )

import Generated.Data exposing (Settings)
import List.Zipper exposing (Zipper)



-- DEFINITIONS


type State data allowed
    = State data


type ProcessingState
    = Preview (State () { toProcessing : Allowed })
    | Ready (State () { toProcessing : Allowed })
    | Processing (State () { toReady : Allowed, toQueued : Allowed })
    | Queued (State FilmRoll { toProcessing : Allowed })


type alias Allowed =
    { allowed : () }


type alias FilmRoll =
    Zipper Settings



-- TRANSITIONS


preview : ProcessingState
preview =
    Preview (State ())


toReady : State b { a | toReady : Allowed } -> ProcessingState
toReady _ =
    Ready (State ())


toProcessing : State b { a | toProcessing : Allowed } -> ProcessingState
toProcessing _ =
    Processing (State ())


toQueued : FilmRoll -> State b { a | toQueued : Allowed } -> ProcessingState
toQueued x _ =
    Queued (State x)


map : (FilmRoll -> FilmRoll) -> State FilmRoll a -> ProcessingState
map f (State a) =
    Queued (State (f a))


toData : State a b -> a
toData (State a) =
    a
