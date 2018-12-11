port module Main exposing (main)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode as D
import Json.Encode as E


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
    , debug : String
    }


idDecoder : D.Decoder String
idDecoder =
    D.field "id" D.string


nameDecoder : D.Decoder String
nameDecoder =
    D.field "name" D.string


costDecoder : D.Decoder Int
costDecoder =
    D.field "cost" D.int


budgetItemIdDecoder : D.Decoder String
budgetItemIdDecoder =
    D.field "budgetItemId" D.string


itemDecoder : D.Decoder BudgetItem
itemDecoder =
    D.map3 BudgetItem
        idDecoder
        nameDecoder
        costDecoder


transactionDecoder : D.Decoder Transaction
transactionDecoder =
    D.map3 Transaction
        idDecoder
        budgetItemIdDecoder
        costDecoder


itemsDecoder : D.Decoder (List BudgetItem)
itemsDecoder =
    D.field "items" (D.list itemDecoder)


transactionsDecoder : D.Decoder (List Transaction)
transactionsDecoder =
    D.field "transactions" (D.list transactionDecoder)


type alias State =
    { items : List BudgetItem
    , transactions : List Transaction
    }


stateDecoder : D.Decoder State
stateDecoder =
    D.map2 State
        itemsDecoder
        transactionsDecoder


init : () -> ( Model, Cmd Msg )
init _ =
    ( { items = []
      , transactions = []
      , newTransactionValue = 0
      , commited = False
      , debug = ""
      }
    , Cmd.none
    )


port portIntoElm : (E.Value -> msg) -> Sub msg


subscriptions : Model -> Sub Msg
subscriptions model =
    portIntoElm GotValueFromJS


type Msg
    = Noop
    | GotValueFromJS E.Value
    | AddItem
    | ChangeItemName BudgetItem String
    | ChangeItemCost BudgetItem Int
    | DeleteItem BudgetItem
    | Commit
    | RemoveTransaction Transaction
    | ChangeTransactionCost Transaction Int
    | ChangeNewTransactionValue Int
    | AddTransaction String Int


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


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Noop ->
            ( model
            , Cmd.none
            )

        GotValueFromJS val ->
            case D.decodeValue stateDecoder val of
                Err err ->
                    ( { model | debug = D.errorToString err }, Cmd.none )

                Ok decoded ->
                    ( { items = decoded.items
                      , transactions = decoded.transactions
                      , commited = False
                      , newTransactionValue = 0
                      , debug = ""
                      }
                    , Cmd.none
                    )

        AddItem ->
            let
                item =
                    { id = String.fromInt (List.length model.items)
                    , name = ""
                    , cost = 0
                    }
            in
            ( { model
                | items = model.items ++ [ item ]
              }
            , Cmd.none
            )

        ChangeItemName item value ->
            ( { model
                | items =
                    updateById
                        item.id
                        (\x -> { x | name = value })
                        model.items
              }
            , Cmd.none
            )

        ChangeItemCost item cost ->
            ( { model
                | items =
                    updateById
                        item.id
                        (\x -> { x | cost = cost })
                        model.items
              }
            , Cmd.none
            )

        DeleteItem item ->
            ( { model
                | items =
                    List.filter
                        (\x -> item /= x)
                        model.items
              }
            , Cmd.none
            )

        -- TODO: Remove empty rows
        -- TODO: Validate rows
        Commit ->
            ( { model | commited = True }
            , Cmd.none
            )

        RemoveTransaction transaction ->
            ( { model
                | transactions =
                    List.filter
                        (\x -> x.id /= transaction.id)
                        model.transactions
              }
            , Cmd.none
            )

        ChangeTransactionCost transaction cost ->
            ( { model
                | transactions =
                    updateById
                        transaction.id
                        (\x -> { x | cost = cost })
                        model.transactions
              }
            , Cmd.none
            )

        AddTransaction itemId value ->
            if value == 0 then
                ( model, Cmd.none )

            else
                ( { model
                    | transactions =
                        model.transactions
                            ++ [ { id = String.fromInt <| List.length model.transactions
                                 , budgetItemId = itemId
                                 , cost = value
                                 }
                               ]
                    , newTransactionValue = 0
                  }
                , Cmd.none
                )

        ChangeNewTransactionValue value ->
            ( { model | newTransactionValue = value }, Cmd.none )


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


viewCommitedBudgetItem : Int -> BudgetItem -> List Transaction -> Html Msg
viewCommitedBudgetItem newTransValue item transactions =
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
                    , onBlur (AddTransaction item.id newTransValue)
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
                    viewCommitedBudgetItem
                        model.newTransactionValue
                        item
                        (getTransactions item)
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
    Browser.element
        { init = init
        , subscriptions = subscriptions
        , update = update
        , view = view
        }
