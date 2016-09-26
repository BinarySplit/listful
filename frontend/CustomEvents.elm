module CustomEvents exposing (onKeyCombo)

import Msg
import Json.Decode exposing (..)
import Html exposing (Attribute)
import Html.Events exposing (onWithOptions)


onKeyCombo : (( Bool, Bool, Bool, Int ) -> Result String a) -> Attribute a
onKeyCombo handler =
    onWithOptions "keypress"
        { stopPropagation = False, preventDefault = True }
        (customDecoder decodeKeyPress handler)


decodeKeyPress : Decoder ( Bool, Bool, Bool, Int )
decodeKeyPress =
    object4 (,,,)
        (at [ "ctrlKey" ] bool)
        (at [ "altKey" ] bool)
        (at [ "shiftKey" ] bool)
        (at [ "keyCode" ] int)
