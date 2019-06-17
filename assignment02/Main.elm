module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)


main =
    Html.beginnerProgram
        { view = view
        , model = init
        , update = update
        }


init : Model
init =
    { currentYear = ""
    , count = 0
    , resultList = []
    }


type alias Model =
    { currentYear : String
    , count : Int
    , resultList : List String
    }


type Msg
    = UpdateQuery String
    | FindLeapYear String Int (List String)


findLeapYearFunc : String -> Int -> List String -> List String
findLeapYearFunc currentYear count resultList =
    if count < 20 then
        if (((String.trim currentYear |> String.toInt) |> Result.withDefault 0) % 400) == 0 || ((((String.trim currentYear |> String.toInt) |> Result.withDefault 0) % 4) == 0 && (((String.trim currentYear |> String.toInt) |> Result.withDefault 0) % 100) /= 0) then
            --List.append resultList currentYear |>
            findLeapYearFunc (toString (((String.trim currentYear |> String.toInt) |> Result.withDefault 0) + 1)) (count + 1) (List.append resultList (String.split "," currentYear))
        else
            findLeapYearFunc (toString (((String.trim currentYear |> String.toInt) |> Result.withDefault 0) + 1)) count resultList
    else
        resultList


update : Msg -> Model -> Model
update msg model =
    case msg of
        UpdateQuery currentYear ->
            { model | currentYear = currentYear, resultList = [] }

        FindLeapYear currentYear count resultList ->
            { model | resultList = findLeapYearFunc currentYear count resultList }


viewListResult : String -> Html Msg
viewListResult record =
    li [] [ text record ]


renderList : List String -> Html Msg
renderList resultList =
    ul [] (List.map (\l -> li [] [ text l ]) resultList)


view : Model -> Html Msg
view model =
    div [ class "main-container" ]
        [ header []
            [ h1 [] [ text "Assignment 2" ]
            ]
        , table []
            [ tr [] [ td [] [ text "Enter currentYear :" ], td [] [ input [ class "inputCls", placeholder "Enter current year in YYYY format..", onInput UpdateQuery ] [] ] ] ]
        , button [ class "srchBtn", onClick <| FindLeapYear model.currentYear model.count model.resultList ] [ text "Find Next 20 Leap Year" ]
        , renderList model.resultList
        ]
