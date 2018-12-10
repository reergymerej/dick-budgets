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


type alias Transaction =
    { id : String
    , budgetItemId : String
    , cost : Int
    }


type alias Model =
    { items : List BudgetItem
    , commited : Bool
    , transactions : List Transaction
    , newTransactionValue : Int
    }


init : Model
init =
    { items =
        [ { id = "0", name = "XYZ", cost = 100 }
        , { id = "1", name = "ABC", cost = 24 }
        , { id = "2", name = "FOO", cost = 155 }
        , { id = "3", name = "BAR", cost = 30 }
        ]
    , commited = True
    , transactions =
        [ { id = "0", budgetItemId = "0", cost = 50 }
        , { id = "1", budgetItemId = "0", cost = 20 }
        , { id = "2", budgetItemId = "0", cost = 30 }
        , { id = "3", budgetItemId = "1", cost = 12 }
        , { id = "4", budgetItemId = "3", cost = 30 }
        , { id = "5", budgetItemId = "3", cost = 35 }
        ]
    , newTransactionValue = 0
    }


type Msg
    = Noop
    | AddItem
    | ChangeItemName BudgetItem String
    | ChangeItemCost BudgetItem Int
    | DeleteItem BudgetItem
    | Commit
    | RemoveTransaction Transaction
    | ChangeTransactionCost Transaction Int
    | ChangeNewTransactionValue Int
    | AddTransaction String


type alias Idly a =
    { a | id : String }


updateById : String -> (Idly a -> Idly a) -> List (Idly a) -> List (Idly a)
updateById id getNewItem list =
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
        Noop ->
            model

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
                    updateById
                        item.id
                        (\x -> { x | name = value })
                        model.items
            }

        ChangeItemCost item cost ->
            { model
                | items =
                    updateById
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

        -- TODO: Remove empty rows
        -- TODO: Validate rows
        Commit ->
            { model | commited = True }

        RemoveTransaction transaction ->
            { model
                | transactions =
                    List.filter
                        (\x -> x.id /= transaction.id)
                        model.transactions
            }

        ChangeTransactionCost transaction cost ->
            { model
                | transactions =
                    updateById
                        transaction.id
                        (\x -> { x | cost = cost })
                        model.transactions
            }

        AddTransaction itemId ->
            { model
                | transactions =
                    model.transactions
                        ++ [ { id = String.fromInt <| List.length model.transactions
                             , budgetItemId = itemId
                             , cost = model.newTransactionValue
                             }
                           ]
                , newTransactionValue = 0
            }

        ChangeNewTransactionValue value ->
            { model | newTransactionValue = value }


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


viewRow : List (Html Msg) -> Html Msg
viewRow cells =
    div
        [ toClassList "flex" ]
        (List.map
            (\x -> viewCell [ x ])
            cells
        )


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
        , input
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
        , button
            [ onClick (DeleteItem item)
            , toClassList "text-red-light font-bold p-1 px-2 rounded text-sm"
            ]
            [ text "Remove" ]
        ]


viewStyledTotal : String -> Int -> Int -> Html Msg
viewStyledTotal extraStyles base value =
    div
        [ if base > value then
            toClassList (extraStyles ++ " text-green")
          else if base < value then
            toClassList (extraStyles ++ " text-red")
          else
            toClassList (extraStyles ++ " text-black")
        ]
        [ text (String.fromInt value) ]


viewStyledTotalBasic : Int -> Int -> Html Msg
viewStyledTotalBasic base value =
    viewStyledTotal "" base value


viewTransactionItem : Transaction -> Html Msg
viewTransactionItem transaction =
    div []
        [ input
            [ value <| String.fromInt transaction.cost
            , toClassList "outline-none w-full text-right"
            , onInput
                (\val ->
                    if val == "" then
                        RemoveTransaction transaction
                    else
                        case String.toInt val of
                            Nothing ->
                                ChangeTransactionCost transaction transaction.cost

                            Just int ->
                                ChangeTransactionCost transaction int
                )
            ]
            []
        ]


viewCommitedBudgetItem : BudgetItem -> List Transaction -> Html Msg
viewCommitedBudgetItem item transactions =
    let
        transSum =
            sumItems transactions

        transCols =
            List.map
                viewTransactionItem
                transactions
    in
    viewRow
        ([ div [] [ text item.name ]
         , div [] [ text (String.fromInt item.cost) ]
         , viewStyledTotalBasic item.cost transSum
         ]
            ++ transCols
            ++ [ input
                    [ placeholder "+"
                    , onInput
                        (\x ->
                            case String.toInt x of
                                Nothing ->
                                    Noop

                                Just int ->
                                    ChangeNewTransactionValue int
                        )
                    , onBlur (AddTransaction item.id)
                    ]
                    []
               ]
        )


sumItems : List { a | cost : Int } -> Int
sumItems items =
    List.foldl
        (\current accumulator -> current.cost + accumulator)
        0
        items


viewUncommitted : Model -> Html Msg
viewUncommitted model =
    div
        []
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
            , div [ toClassList "text-right text-xl" ]
                [ text <| String.fromInt <| sumItems model.items ]
            , button
                [ onClick Commit
                , toClassList "bg-indigo font-bold px-4 py-2 rounded text-white"
                ]
                [ text "Commit" ]
            ]
        ]


transactionsFor : List Transaction -> BudgetItem -> List Transaction
transactionsFor transactions budgetItem =
    List.filter (\x -> x.budgetItemId == budgetItem.id) transactions


viewCommitted : Model -> Html Msg
viewCommitted model =
    let
        getTransactions =
            transactionsFor model.transactions

        budgetSum =
            sumItems model.items
    in
    div []
        [ div
            []
            (List.map
                (\item ->
                    viewCommitedBudgetItem item (getTransactions item)
                )
                model.items
            )
        , viewRow
            [ div [] []
            , div [ toClassList "text-xl" ] [ text <| String.fromInt budgetSum ]
            , viewStyledTotal "text-xl" budgetSum <| sumItems model.transactions
            ]
        ]


view : Model -> Html Msg
view model =
    div
        [ toClassList "font-mono p-4" ]
        [ if not model.commited then
            viewUncommitted model
          else
            viewCommitted model
        ]


main =
    Browser.sandbox
        { init = init
        , update = update
        , view = view
        }
