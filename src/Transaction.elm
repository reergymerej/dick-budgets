module Transaction exposing (T, create, encoder, transactionsDecoder, transactionsFor)

import Json.Decode as D
import Json.Encode as E
import Util


type alias T =
    { id : String
    , parent : String
    , cost : Int
    }


idDecoder : D.Decoder String
idDecoder =
    D.field "id" D.string


budgetItemIdDecoder : D.Decoder String
budgetItemIdDecoder =
    D.field "parent" D.string


costDecoder : D.Decoder Int
costDecoder =
    D.field "cost" D.int


decoder : D.Decoder T
decoder =
    D.map3 T
        idDecoder
        budgetItemIdDecoder
        costDecoder


transactionsDecoder : D.Decoder (List T)
transactionsDecoder =
    D.field "transactions" (D.list decoder)


encoder : T -> E.Value
encoder x =
    E.object
        [ ( "id", E.string x.id )
        , ( "parent", E.string x.parent )
        , ( "cost", E.int x.cost )
        ]


transactionsFor : List T -> { a | id : String } -> List T
transactionsFor transactions items =
    List.filter (\x -> x.parent == items.id) transactions


create : List T -> String -> Int -> T
create transactions parentId value =
    { id = Util.getNextId transactions
    , parent = parentId
    , cost = value
    }
