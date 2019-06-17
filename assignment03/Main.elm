module Main exposing (..)

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
    { query : String
    , result : String
    }


init : Model
init =
    { query = ""
    , result = ""
    }


type Msg
    = UpdateQuery String
    | ComputeSum String
    | ComputeProduct String


calculateSum : String -> Int
calculateSum number =
    String.toInt number
        |> Result.withDefault 0
        |> List.range 1
        |> List.sum


calculateProduct : String -> Int
calculateProduct number =
    String.toInt number
        |> Result.withDefault 0
        |> List.range 1
        |> List.product


update : Msg -> Model -> Model
update msg model =
    case msg of
        UpdateQuery query ->
            { model | query = query }

        ComputeSum query ->
            { model | result = calculateSum query |> toString }

        ComputeProduct query ->
            { model | result = calculateProduct query |> toString }


view : Model -> Html Msg
view model =
    div [ class "main-container" ]
        [ header []
            [ h1 [] [ text "Assignment 3" ]
            , table []
                [ tr []
                    [ td [] [ text "Enter Number : " ]
                    , td [] [ input [ class "inputCls", placeholder "Enter Number N..", onInput UpdateQuery ] [] ]
                    ]
                ]
            , button [ class "srchBtn", onClick <| ComputeSum model.query ] [ text "Compute Sum" ]
            , br [] []
            , button [ class "srchBtn", onClick <| ComputeProduct model.query ] [ text "Compute Product" ]
            , text model.result
            ]
        ]
