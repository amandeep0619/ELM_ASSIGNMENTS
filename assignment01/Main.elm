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
    { inputList : String
    , member : String
    , outputResult : String
    }


init : Model
init =
    { inputList = ""
    , member = ""
    , outputResult = ""
    }


type Msg
    = UpdateList String
    | UpdateMember String
    | FindMember String String


update : Msg -> Model -> Model
update msg model =
    case msg of
        UpdateList inputList ->
            { model | inputList = inputList }

        UpdateMember member ->
            { model | member = member }

        FindMember inputList member ->
            { model | outputResult = checkMemberInList inputList member }


checkMemberInList : String -> String -> String
checkMemberInList inputString member =
    let
        checkMember =
            List.member member (String.split "," inputString |> List.map String.trim)
    in
    if checkMember && member /= "" then
        "Member Exists"
    else if member /= "" then
        "Member doesn't Exists"
    else
        ""


view : Model -> Html Msg
view model =
    div [ class "main-container" ]
        [ header []
            [ h1 [] [ text "ELM Assignment 1" ]
            ]
        , table []
            [ tr []
                [ td [] [ text "Enter List :" ]
                , td [] [ input [ class "inputCls", placeholder "Enter comma separated String as List ..", onInput UpdateList ] [] ]

                --    , td [] [ text <| "List is : " ++ model.inputList ]
                ]
            , tr []
                [ td [] [ text "Enter Member :" ]
                , td []
                    [ input [ class "inputCls", placeholder "Enter element to search in List .. ", onInput UpdateMember ] []
                    ]

                --    , td [] [ text <| "List is : " ++ model.inputList ]
                ]

            --, text <| "List is : " ++ model.inputList
            --  , br [] []
            --  , text "Enter Member"
            --  , input [ class "inputCls", placeholder "Enter element to search in List .. ", onInput UpdateMember ] []
            --  , br [] []
            ]
        , button [ class "srchBtn", onClick <| FindMember model.inputList model.member ] [ text "Search Element" ]
        , text model.outputResult
        ]
