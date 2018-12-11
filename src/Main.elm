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
    { items : List BudgetItem.T
    , committed : Bool
    , transactions : List Transaction.T
    , newTransactionValue : Int
    , debug : String
    }


type alias State =
    { items : List BudgetItem.T
    , transactions : List Transaction.T
    , committed : Bool
    }


stateDecoder : D.Decoder State
stateDecoder =
    D.map3 State
        BudgetItem.itemsDecoder
        Transaction.transactionsDecoder
        (D.field "committed" D.bool)


init : () -> ( Model, Cmd Msg )
init _ =
    ( { items = []
      , transactions = []
      , newTransactionValue = 0
      , committed = False
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
    | ChangeItemName BudgetItem.T String
    | ChangeItemCost BudgetItem.T Int
    | DeleteItem BudgetItem.T
    | ChangeCommit Bool
    | RemoveTransaction Transaction.T
    | ChangeTransactionCost Transaction.T Int
    | ChangeNewTransactionValue Int
    | AddTransaction String Int


serializeModel : Model -> E.Value
serializeModel model =
    E.object
        [ ( "items", E.list BudgetItem.encoder model.items )
        , ( "transactions", E.list Transaction.encoder model.transactions )
        , ( "committed", E.bool model.committed )
        ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Noop ->
            ( model, Cmd.none )

        GotValueFromJS val ->
            case D.decodeValue stateDecoder val of
                Err err ->
                    ( { model | debug = D.errorToString err }, Cmd.none )

                Ok decoded ->
                    ( { items = decoded.items
                      , transactions = decoded.transactions
                      , committed = decoded.committed
                      , newTransactionValue = 0
                      , debug = ""
                      }
                    , Cmd.none
                    )

        AddItem ->
            let
                item =
                    BudgetItem.create model.items

                newModel =
                    { model | items = model.items ++ [ item ] }
            in
            ( newModel, portOutOfElm <| serializeModel newModel )

        ChangeItemName item value ->
            let
                updateName =
                    Util.updateById (\x -> { x | name = value })

                newModel =
                    { model | items = updateName item.id model.items }
            in
            ( newModel, portOutOfElm <| serializeModel newModel )

        ChangeItemCost item cost ->
            let
                updateCost =
                    Util.updateById (\x -> { x | cost = cost })

                newModel =
                    { model | items = updateCost item.id model.items }
            in
            ( newModel, portOutOfElm <| serializeModel newModel )

        ChangeTransactionCost transaction cost ->
            let
                updateCost =
                    Util.updateById (\x -> { x | cost = cost })

                newModel =
                    { model | transactions = updateCost transaction.id model.transactions }
            in
            ( newModel, portOutOfElm <| serializeModel newModel )

        DeleteItem item ->
            let
                newModel =
                    { model
                        | items =
                            List.filter (\x -> item /= x) model.items
                    }
            in
            ( newModel, portOutOfElm <| serializeModel newModel )

        ChangeCommit committed ->
            let
                newModel =
                    { model | committed = committed }
            in
            ( newModel, portOutOfElm <| serializeModel newModel )

        RemoveTransaction transaction ->
            let
                newModel =
                    { model
                        | transactions =
                            List.filter (\x -> x.id /= transaction.id) model.transactions
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
        [ toClassList "w-32 p-3 inline-block" ]
        content


viewRow : List (Html Msg) -> Html Msg
viewRow cells =
    div
        [ toClassList "whitespace-no-wrap" ]
        (List.map (\x -> viewCell [ x ]) cells)


viewBugetItem : BudgetItem.T -> Html Msg
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


viewTransactionItem : Transaction.T -> Html Msg
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


viewCommitedBudgetItem : Int -> BudgetItem.T -> List Transaction.T -> Html Msg
viewCommitedBudgetItem newTransValue item transactions =
    let
        transactionsColumns =
            List.map
                viewTransactionItem
                transactions
    in
    viewRow
        ([ div [] [ text item.name ]
         , div [] [ text (String.fromInt item.cost) ]
         , viewStyledTotalBasic item.cost <| Util.sumItems transactions
         ]
            ++ transactionsColumns
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
                [ text <| String.fromInt <| Util.sumItems model.items ]
            ]
        ]


viewCommitted : Model -> Html Msg
viewCommitted model =
    let
        getTransactions =
            Transaction.transactionsFor model.transactions

        budgetSum =
            Util.sumItems model.items
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
            , viewStyledTotal "text-xl" budgetSum <| Util.sumItems model.transactions
            ]
        ]


viewControls : Model -> Html Msg
viewControls model =
    div []
        (if not model.committed then
            [ button
                [ onClick (ChangeCommit True)
                , toClassList "bg-indigo font-bold px-4 py-2 rounded text-white"
                ]
                [ text "Commit" ]
            ]
         else
            [ button
                [ onClick (ChangeCommit False)
                , toClassList "bg-indigo font-bold px-4 py-2 rounded text-white"
                ]
                [ text "Un-Commit" ]
            ]
        )


view : Model -> Html Msg
view model =
    div
        [ toClassList "font-mono p-4" ]
        [ viewControls model
        , if not model.committed then
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
