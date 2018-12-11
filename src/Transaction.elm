module Transaction exposing (..)

import BudgetItem
import Json.Decode as D
import Json.Encode as E


type alias Transaction =
    { id : String
    , budgetItemId : String
    , cost : Int
    }


budgetItemIdDecoder : D.Decoder String
budgetItemIdDecoder =
    D.field "budgetItemId" D.string


decoder : D.Decoder Transaction
decoder =
    D.map3 Transaction
        -- This is just a generic decoder.
        BudgetItem.idDecoder
        budgetItemIdDecoder
        -- This is just a generic decoder.
        BudgetItem.costDecoder


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


transactionsFor : List Transaction -> BudgetItem.BudgetItem -> List Transaction
transactionsFor transactions budgetItem =
    List.filter (\x -> x.budgetItemId == budgetItem.id) transactions
