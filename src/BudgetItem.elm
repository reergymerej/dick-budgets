module BudgetItem exposing (T, create, encoder, itemsDecoder)

import Json.Decode as D
import Json.Encode as E
import Util


type alias T =
    { id : String
    , name : String
    , cost : Int
    }


costDecoder : D.Decoder Int
costDecoder =
    D.field "cost" D.int


idDecoder : D.Decoder String
idDecoder =
    D.field "id" D.string


nameDecoder : D.Decoder String
nameDecoder =
    D.field "name" D.string


decoder : D.Decoder T
decoder =
    D.map3 T
        idDecoder
        nameDecoder
        costDecoder


itemsDecoder : D.Decoder (List T)
itemsDecoder =
    D.field "items" (D.list decoder)


encoder : T -> E.Value
encoder x =
    E.object
        [ ( "id", E.string x.id )
        , ( "name", E.string x.name )
        , ( "cost", E.int x.cost )
        ]


create : List T -> T
create items =
    { id = Util.getNextId items
    , name = ""
    , cost = 0
    }
