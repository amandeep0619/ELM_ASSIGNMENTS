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
    { inputNumber : String
    , resultList : List String
    }


init : Model
init =
    { inputNumber = ""
    , resultList = []
    }


type Msg
    = UpdateQuery String
    | ConvertNumberToList String (List String)


convertNumberToListFunc : String -> List String -> List String
convertNumberToListFunc inputNumber resultList =
    let
        number =
            Result.withDefault 0 (String.toInt inputNumber)
    in
    if number > 0 then
        convertNumberToListFunc (toString (number // 10)) (List.append resultList (String.split "," (toString (number % 10))))
    else
        List.foldl (::) [] resultList


update : Msg -> Model -> Model
update msg model =
    case msg of
        UpdateQuery inputNumber ->
            { model | inputNumber = inputNumber, resultList = [] }

        ConvertNumberToList inputNumber resultList ->
            { model
                | resultList = []
                , resultList = convertNumberToListFunc inputNumber resultList
            }


view : Model -> Html Msg
view model =
    div [ class "main-container" ]
        [ header []
            [ h1 [] [ text "ELM Assignment 5" ]
            ]
        , table []
            [ tr []
                [ td [] [ text "Enter Number :" ]
                , td [] [ input [ class "inputCls", placeholder "Enter number 123456...", onInput UpdateQuery ] [] ]

                --    , td [] [ text <| "List is : " ++ model.inputList ]
                ]

            --, text <| "List is : " ++ model.inputList
            --  , br [] []
            --  , text "Enter Member"
            --  , input [ class "inputCls", placeholder "Enter element to search in List .. ", onInput UpdateMember ] []
            --  , br [] []
            ]
        , button [ class "srchBtn", onClick <| ConvertNumberToList model.inputNumber model.resultList ] [ text "Search Element" ]
        , text (toString model.resultList)
        ]
