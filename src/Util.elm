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
