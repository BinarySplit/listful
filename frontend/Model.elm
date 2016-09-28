module Model exposing (Model, NodeList, Node, model, init)

import Dict exposing (Dict, insert, empty)
import Msg exposing (Msg)


type alias Model =
    { nodes : NodeList
    , nextId : Int
    , focusId : Maybe Int
    }


type alias NodeList =
    Dict Int Node


type alias Node =
    { content : String
    , childrenIds : List Int
    }


model : Model
model =
    { nodes =
        Dict.fromList
            [ ( 0, { content = "hi", childrenIds = [] } )
            ]
    , nextId = 1
    , focusId = Just 0
    }


init : ( Model, Cmd Msg )
init =
    model ! []
