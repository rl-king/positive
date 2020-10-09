module Icon exposing
    ( applyAll
    , applyBoth
    , applyCrop
    , applyTone
    , cancel
    , copy
    , crop
    , highres
    , left
    , ok
    , original
    , reset
    , resetTone
    , right
    , rotate
    , save
    , undo
    , wallpaper
    )

import Html exposing (..)
import Html.Attributes as Attributes exposing (..)


left : Html msg
left =
    img [ src "/dist/icons/left.svg" ] []


right : Html msg
right =
    img [ src "/dist/icons/right.svg" ] []


ok : Html msg
ok =
    img [ src "/dist/icons/ok.svg" ] []


applyAll : Html msg
applyAll =
    img [ src "/dist/icons/apply-all.svg" ] []


applyBoth : Html msg
applyBoth =
    img [ src "/dist/icons/apply-both.svg" ] []


applyTone : Html msg
applyTone =
    img [ src "/dist/icons/apply-tone.svg" ] []


applyCrop : Html msg
applyCrop =
    img [ src "/dist/icons/apply-crop.svg" ] []


cancel : Html msg
cancel =
    img [ src "/dist/icons/cancel.svg" ] []


crop : Html msg
crop =
    img [ src "/dist/icons/crop.svg" ] []


rotate : Html msg
rotate =
    img [ src "/dist/icons/rotate.svg" ] []


copy : Html msg
copy =
    img [ src "/dist/icons/copy.svg" ] []


save : Html msg
save =
    img [ src "/dist/icons/save.svg" ] []


original : Html msg
original =
    img [ src "/dist/icons/original.svg" ] []


wallpaper : Html msg
wallpaper =
    img [ src "/dist/icons/wallpaper.svg" ] []


highres : Html msg
highres =
    img [ src "/dist/icons/highres.svg" ] []


reset : Html msg
reset =
    img [ src "/dist/icons/reset.svg" ] []


resetTone : Html msg
resetTone =
    img [ src "/dist/icons/reset-tone.svg" ] []


undo : Html msg
undo =
    img [ src "/dist/icons/undo.svg" ] []
