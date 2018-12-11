port module Main exposing (main)

import Browser
import BudgetItem
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode as D
import Json.Encode as E
import Transaction
import Util


type alias Model =
    { items : List BudgetItem.BudgetItem
    , commited : Bool
    , transactions : List Transaction.Transaction
    , newTransactionValue : Int
    , debug : String
    }


type alias State =
    { items : List BudgetItem.BudgetItem
    , transactions : List Transaction.Transaction
    }


stateDecoder : D.Decoder State
stateDecoder =
    D.map2 State
        BudgetItem.itemsDecoder
        Transaction.transactionsDecoder


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


port portOutOfElm : E.Value -> Cmd msg


subscriptions : Model -> Sub Msg
subscriptions model =
    portIntoElm GotValueFromJS


type Msg
    = Noop
    | GotValueFromJS E.Value
    | AddItem
    | ChangeItemName BudgetItem.BudgetItem String
    | ChangeItemCost BudgetItem.BudgetItem Int
    | DeleteItem BudgetItem.BudgetItem
    | Commit
    | RemoveTransaction Transaction.Transaction
    | ChangeTransactionCost Transaction.Transaction Int
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


serializeModel : Model -> E.Value
serializeModel model =
    E.object
        [ ( "items", E.list BudgetItem.encoder model.items )
        , ( "transactions", E.list Transaction.encoder model.transactions )
        ]


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
                    { id = Util.getNextId model.items
                    , name = ""
                    , cost = 0
                    }

                newModel =
                    { model | items = model.items ++ [ item ] }
            in
            ( newModel, portOutOfElm <| serializeModel newModel )

        ChangeItemName item value ->
            let
                newModel =
                    { model
                        | items =
                            updateById
                                item.id
                                (\x -> { x | name = value })
                                model.items
                    }
            in
            ( newModel, portOutOfElm <| serializeModel newModel )

        ChangeItemCost item cost ->
            let
                newModel =
                    { model
                        | items =
                            updateById
                                item.id
                                (\x -> { x | cost = cost })
                                model.items
                    }
            in
            ( newModel, portOutOfElm <| serializeModel newModel )

        DeleteItem item ->
            let
                newModel =
                    { model
                        | items =
                            List.filter
                                (\x -> item /= x)
                                model.items
                    }
            in
            ( newModel, portOutOfElm <| serializeModel newModel )

        -- TODO: Remove empty rows
        -- TODO: Validate rows
        Commit ->
            ( { model | commited = True }
            , Cmd.none
            )

        RemoveTransaction transaction ->
            let
                newModel =
                    { model
                        | transactions =
                            List.filter
                                (\x -> x.id /= transaction.id)
                                model.transactions
                    }
            in
            ( newModel, portOutOfElm <| serializeModel newModel )

        ChangeTransactionCost transaction cost ->
            let
                newModel =
                    { model
                        | transactions =
                            updateById
                                transaction.id
                                (\x -> { x | cost = cost })
                                model.transactions
                    }
            in
            ( newModel, portOutOfElm <| serializeModel newModel )

        AddTransaction itemId value ->
            if value == 0 then
                ( model, Cmd.none )
            else
                let
                    newModel =
                        { model
                            | transactions =
                                model.transactions
                                    ++ [ Transaction.create
                                            model.transactions
                                            itemId
                                            value
                                       ]
                            , newTransactionValue = 0
                        }
                in
                ( newModel, portOutOfElm <| serializeModel newModel )

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


viewBugetItem : BudgetItem.BudgetItem -> Html Msg
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


viewTransactionItem : Transaction.Transaction -> Html Msg
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


viewCommitedBudgetItem : Int -> BudgetItem.BudgetItem -> List Transaction.Transaction -> Html Msg
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


viewCommitted : Model -> Html Msg
viewCommitted model =
    let
        getTransactions =
            Transaction.transactionsFor model.transactions

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
