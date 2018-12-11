module BudgetItem exposing (..)

import Json.Decode as D
import Json.Encode as E


type alias BudgetItem =
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


decoder : D.Decoder BudgetItem
decoder =
    D.map3 BudgetItem
        idDecoder
        nameDecoder
        costDecoder


itemsDecoder : D.Decoder (List BudgetItem)
itemsDecoder =
    D.field "items" (D.list decoder)


encoder : BudgetItem -> E.Value
encoder x =
    E.object
        [ ( "id", E.string x.id )
        , ( "name", E.string x.name )
        , ( "cost", E.int x.cost )
        ]
