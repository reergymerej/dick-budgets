module Main exposing (main)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)


type alias BudgetItem =
    { id : String
    , name : String
    , cost : Float
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
    | ChangeItemCost BudgetItem Float
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
                    , cost = 0.0
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


viewBugetItem : BudgetItem -> Html Msg
viewBugetItem item =
    div
        [ toClassList "p-4"
        ]
        [ input
            [ value item.name
            , placeholder "Name"
            , onInput (ChangeItemName item)
            , toClassList "outline-none"
            ]
            []
        , input
            [ value (String.fromFloat item.cost)
            , placeholder "Cost"
            , onInput
                (\val ->
                    case String.toFloat val of
                        Nothing ->
                            ChangeItemCost item item.cost

                        Just float ->
                            ChangeItemCost item float
                )
            , toClassList "outline-none"
            ]
            []
        , button
            [ onClick (DeleteItem item)
            , toClassList "bg-pink-dark font-bold p-1 rounded text-white text-sm w-5"
            ]
            [ text "X" ]
        ]


viewBugetItems : Model -> Html Msg
viewBugetItems model =
    div
        [ toClassList "border-b mb-4"
        ]
        (List.map
            (\item -> viewBugetItem item)
            model.items
        )


view : Model -> Html Msg
view model =
    div
        [ toClassList "font-mono"
        ]
        [ viewBugetItems model
        , button
            [ onClick AddItem
            , toClassList "bg-indigo font-bold px-4 py-2 rounded text-white"
            ]
            [ text "Add" ]
        , div []
            [ text
                (String.fromFloat
                    (List.foldl
                        (\current accumulator -> current.cost + accumulator)
                        0
                        model.items
                    )
                )
            ]
        ]


main =
    Browser.sandbox
        { init = init
        , update = update
        , view = view
        }
