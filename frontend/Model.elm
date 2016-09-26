module Model exposing (Model, Node, model)

import Dict exposing (Dict, insert, empty)


type alias Model =
    { nodes : Dict Int Node
    , nextId : Int
    }


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
    }
