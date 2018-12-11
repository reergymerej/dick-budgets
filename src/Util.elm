module Util exposing (..)


getLastItem : List a -> Maybe a
getLastItem list =
    List.head <| List.reverse list


getNextId : List { a | id : String } -> String
getNextId list =
    case getLastItem list of
        Nothing ->
            "0"

        Just item ->
            case String.toInt item.id of
                Nothing ->
                    "0"

                Just int ->
                    String.fromInt (int + 1)


sumItems : List { a | cost : Int } -> Int
sumItems items =
    List.foldl
        (\current accumulator -> current.cost + accumulator)
        0
        items


type alias Idly a =
    { a | id : String }


updateById : (Idly a -> Idly a) -> String -> List (Idly a) -> List (Idly a)
updateById getNewItem id list =
    List.map
        (\x ->
            if x.id == id then
                getNewItem x
            else
                x
        )
        list
