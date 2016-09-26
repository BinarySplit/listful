module Main exposing (..)

import Html.App as App
import Model exposing (model)
import Update exposing (update)
import View exposing (view)


main =
    App.beginnerProgram { model = model, view = view, update = update }



-- MODEL
-- UPDATE
-- VIEW
