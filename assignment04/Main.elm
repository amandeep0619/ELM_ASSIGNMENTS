module Main exposing (main)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)


main =
    Html.beginnerProgram
        { model = init
        , view = view
        , update = update
        }


type alias Model =
    { inputList1 : String
    , inputList2 : String
    , maxElement : String
    , finalList : String
    , resultList : List String
    , fibonacciList : List String
    , fib1 : Int
    , fib2 : Int
    , fibCount : Int
    }


init : Model
init =
    { inputList1 = ""
    , inputList2 = ""
    , maxElement = ""
    , finalList = ""
    , resultList = []
    , fibonacciList = []
    , fib1 = 0
    , fib2 = 1
    , fibCount = 0
    }


type Msg
    = UpdateList1 String
    | UpdateList2 String
    | PrintList String
    | FindMaxElement String
    | MergeTwoList String String (List String)
    | Print100FibonacciNumber Int Int Int (List String)


mergeList : List String -> List String -> List String -> List String
mergeList inputList1 inputList2 resultList =
    if List.length inputList1 > 0 && List.length inputList2 > 0 then
        mergeList (Maybe.withDefault [] (List.tail inputList1)) (Maybe.withDefault [] (List.tail inputList2)) (List.append (List.append resultList (String.split "," (Maybe.withDefault "" (List.head inputList1)))) (String.split "," (Maybe.withDefault "" (List.head inputList2))))
    else if List.length inputList1 > 0 then
        mergeList (Maybe.withDefault [] (List.tail inputList1)) [] (List.append resultList (String.split "," (Maybe.withDefault "" (List.head inputList1))))
    else if List.length inputList2 > 0 then
        mergeList (Maybe.withDefault [] (List.tail inputList2)) [] (List.append resultList (String.split "," (Maybe.withDefault "" (List.head inputList2))))
    else
        resultList


printFibonacciList : Int -> Int -> Int -> List String -> List String
printFibonacciList fib1 fib2 fibCount fibonacciList =
    if fibCount == 100 then
        fibonacciList
    else if fibCount == 0 then
        printFibonacciList fib1 fib2 (fibCount + 1) (List.append fibonacciList (List.singleton (toString fib1)))
    else if fibCount == 1 then
        printFibonacciList fib1 fib2 (fibCount + 1) (List.append fibonacciList (List.singleton (toString fib2)))
    else
        printFibonacciList fib2 (fib1 + fib2) (fibCount + 1) (List.append fibonacciList (List.singleton (toString (fib1 + fib2))))


update : Msg -> Model -> Model
update msg model =
    case msg of
        UpdateList1 inputList1 ->
            { model | inputList1 = inputList1 }

        PrintList inputList1 ->
            { model | inputList1 = inputList1 }

        UpdateList2 inputList2 ->
            { model | inputList2 = inputList2 }

        FindMaxElement inputList1 ->
            { model | maxElement = fetchTheMaxElement inputList1 }

        Print100FibonacciNumber fib1 fib2 fibCount fibonacciList ->
            { model
                | fibonacciList = printFibonacciList fib1 fib2 fibCount fibonacciList
            }

        MergeTwoList inputList1 inputList2 resultList ->
            { model | resultList = mergeList (String.split "," inputList1 |> List.map String.trim) (String.split "," inputList2 |> List.map String.trim) resultList }


fetchTheMaxElement : String -> String
fetchTheMaxElement inputList =
    let
        myList =
            List.map String.trim (String.split "," inputList)
    in
    List.map (\l -> Result.withDefault 0 (String.toInt l)) myList
        |> List.sortWith flippedComparison
        |> List.head
        |> Maybe.withDefault 0
        |> toString


flippedComparison a b =
    case compare a b of
        LT ->
            GT

        EQ ->
            EQ

        GT ->
            LT


viewList : List String -> Html Msg
viewList inputList1 =
    ul []
        (List.map (\l -> p [] [ span [] [ text ("* " ++ l ++ " *") ], br [] [] ]) inputList1)


view : Model -> Html Msg
view model =
    div [ class "main-container" ]
        [ header []
            [ h1 [] [ text "ELM Assignment 4" ]
            ]
        , table []
            [ tr []
                [ td [] [ text "Enter List 1:" ]
                , td []
                    [ input [ class "inputCls", placeholder "Enter comma separated String as List ..", onInput UpdateList1 ] []
                    ]
                ]
            , tr []
                [ td
                    []
                    [ text "Enter List 2:" ]
                , td [] [ input [ class "inputCls", placeholder "Enter comma separated String as List ..", onInput UpdateList2 ] [] ]

                --    , td [] [ text <| "List is : " ++ model.inputList ]
                ]

            --, text <| "List is : " ++ model.inputList
            --  , br [] []
            --  , text "Enter Member"
            --  , input [ class "inputCls", placeholder "Enter element to search in List .. ", onInput UpdateMember ] []
            --  , br [] []
            ]
        , button [ class "srchBtn", onClick <| MergeTwoList model.inputList1 model.inputList2 model.resultList ] [ text "Merge Element" ]
        , button [ class "srchBtn", onClick <| PrintList model.inputList1 ] [ text "Print List 1" ]
        , button [ class "srchBtn", onClick <| Print100FibonacciNumber model.fib1 model.fib2 model.fibCount model.fibonacciList ] [ text "Print 100 Fibonacci numbers" ]
        , br [] []
        , text (model.resultList |> toString)
        , br [] []
        , text "****************************"
        , viewList (String.split "," model.inputList1 |> List.map String.trim)
        , text "****************************"
        , viewList model.fibonacciList
        ]
