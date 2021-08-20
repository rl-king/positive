module Icon exposing
    ( applyAll
    , applyAllCrop
    , applyAllRotate
    , applyAllTone
    , applyBoth
    , applyCrop
    , applyTone
    , cancel
    , copy
    , crop
    , externalEditor
    , highres
    , lambda
    , left
    , ok
    , open
    , original
    , reset
    , resetTone
    , right
    , rotate
    , save
    , starred
    , undo
    , unstarred
    , wallpaper
    )

import Html exposing (..)
import Html.Attributes exposing (..)
import String.Interpolate exposing (interpolate)


icon : String -> Html msg
icon name =
    img [ src (interpolate "/dist/icons/{0}.svg" [ name ]) ] []


left : Html msg
left =
    icon "left"


right : Html msg
right =
    icon "right"


starred : Html msg
starred =
    icon "starred"


unstarred : Html msg
unstarred =
    icon "unstarred"


externalEditor : Html msg
externalEditor =
    icon "ps"


lambda : Html msg
lambda =
    icon "lambda"


ok : Html msg
ok =
    icon "ok"


applyAll : Html msg
applyAll =
    icon "apply-all"


applyAllTone : Html msg
applyAllTone =
    icon "apply-all-tone"


applyAllCrop : Html msg
applyAllCrop =
    icon "apply-all-crop"


applyAllRotate : Html msg
applyAllRotate =
    icon "apply-all-rotate"


applyBoth : Html msg
applyBoth =
    icon "apply-both"


applyTone : Html msg
applyTone =
    icon "apply-tone"


applyCrop : Html msg
applyCrop =
    icon "apply-crop"


cancel : Html msg
cancel =
    icon "cancel"


crop : Html msg
crop =
    icon "crop"


rotate : Html msg
rotate =
    icon "rotate"


copy : Html msg
copy =
    icon "copy"


save : Html msg
save =
    icon "save"


original : Html msg
original =
    icon "original"


wallpaper : Html msg
wallpaper =
    icon "wallpaper"


highres : Html msg
highres =
    icon "highres"


reset : Html msg
reset =
    icon "reset"


resetTone : Html msg
resetTone =
    icon "reset-tone"


undo : Html msg
undo =
    icon "undo"


open : Html msg
open =
    icon "open"
