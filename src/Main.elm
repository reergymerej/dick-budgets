module Main exposing (main)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)


type alias BudgetItem =
    { id : String
    , name : String
    , cost : Int
    }


type alias Model =
    { items : List BudgetItem
    }


init : Model
init =
    { items =
        [ { id = "0"
          , name = "Foo"
          , cost = 100
          }
        , { id = "1"
          , name = "Bar"
          , cost = 240
          }
        ]
    }


type Msg
    = AddItem
    | ChangeItemName BudgetItem String
    | ChangeItemCost BudgetItem Int
    | DeleteItem BudgetItem


updateItem : String -> (BudgetItem -> BudgetItem) -> List BudgetItem -> List BudgetItem
updateItem id getNewItem list =
    List.map
        (\x ->
            if x.id == id then
                getNewItem x

            else
                x
        )
        list


update : Msg -> Model -> Model
update msg model =
    case msg of
        AddItem ->
            let
                item =
                    { id = String.fromInt (List.length model.items)
                    , name = ""
                    , cost = 0
                    }
            in
            { model
                | items = model.items ++ [ item ]
            }

        ChangeItemName item value ->
            { model
                | items =
                    updateItem
                        item.id
                        (\x -> { x | name = value })
                        model.items
            }

        ChangeItemCost item cost ->
            { model
                | items =
                    updateItem
                        item.id
                        (\x -> { x | cost = cost })
                        model.items
            }

        DeleteItem item ->
            { model
                | items =
                    List.filter
                        (\x -> item /= x)
                        model.items
            }


toClassList : String -> Attribute Msg
toClassList string =
    classList
        (List.map
            (\x -> ( x, True ))
            (String.words string)
        )


viewCell : List (Html Msg) -> Html Msg
viewCell content =
    div
        [ toClassList "w-32 p-3" ]
        content


viewRow : List (Html Msg) -> List (Html Msg) -> List (Html Msg) -> Html Msg
viewRow a b c =
    div
        [ toClassList "flex" ]
        [ viewCell a
        , viewCell b
        , viewCell c
        ]


viewBugetItem : BudgetItem -> Html Msg
viewBugetItem item =
    viewRow
        [ input
            [ value item.name
            , placeholder "Name"
            , onInput (ChangeItemName item)
            , toClassList "outline-none w-full"
            ]
            []
        ]
        [ input
            [ value (String.fromInt item.cost)
            , placeholder "Cost"
            , onInput
                (\val ->
                    case String.toInt val of
                        Nothing ->
                            ChangeItemCost item item.cost

                        Just int ->
                            ChangeItemCost item int
                )
            , toClassList "outline-none w-full text-right"
            ]
            []
        ]
        [ button
            [ onClick (DeleteItem item)
            , toClassList "text-red-light font-bold p-1 px-2 rounded text-sm"
            ]
            [ text "Remove" ]
        ]


sumItems : List BudgetItem -> Int
sumItems items =
    List.foldl
        (\current accumulator -> current.cost + accumulator)
        0
        items


view : Model -> Html Msg
view model =
    div
        [ toClassList "font-mono p-4" ]
        [ div
            []
            (List.map
                (\item -> viewBugetItem item)
                model.items
            )
        , viewRow
            [ button
                [ onClick AddItem
                , toClassList "bg-green-light font-bold px-4 py-2 rounded text-white"
                ]
                [ text "Add" ]
            ]
            [ div [ toClassList "text-right text-xl" ]
                [ text
                    (String.fromInt
                        (sumItems model.items)
                    )
                ]
            ]
            []
        ]


main =
    Browser.sandbox
        { init = init
        , update = update
        , view = view
        }
