module Transaction exposing (..)

import Json.Decode as D
import Json.Encode as E


type alias Transaction =
    { id : String
    , budgetItemId : String
    , cost : Int
    }


idDecoder : D.Decoder String
idDecoder =
    D.field "id" D.string


budgetItemIdDecoder : D.Decoder String
budgetItemIdDecoder =
    D.field "budgetItemId" D.string


costDecoder : D.Decoder Int
costDecoder =
    D.field "cost" D.int


decoder : D.Decoder Transaction
decoder =
    D.map3 Transaction
        idDecoder
        budgetItemIdDecoder
        costDecoder


transactionsDecoder : D.Decoder (List Transaction)
transactionsDecoder =
    D.field "transactions" (D.list decoder)


encoder : Transaction -> E.Value
encoder x =
    E.object
        [ ( "id", E.string x.id )
        , ( "budgetItemId", E.string x.budgetItemId )
        , ( "cost", E.int x.cost )
        ]


transactionsFor : List Transaction -> { a | id : String } -> List Transaction
transactionsFor transactions items =
    List.filter (\x -> x.budgetItemId == items.id) transactions
