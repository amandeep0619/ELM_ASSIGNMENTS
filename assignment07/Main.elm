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
    , candidate : Int
    , count : Int
    , result : String
    }


init : Model
init =
    { inputNumber = ""
    , candidate = 2
    , count = 0
    , result = ""
    }


type Msg
    = UpdateQuery String
    | FindTheNthPrimeNumber String Int Int


isPrimeNumberFunc : Int -> Int -> Bool
isPrimeNumberFunc number counter =
    if counter < number && number % counter == 0 then
        False
    else if counter == number then
        True
    else
        isPrimeNumberFunc number (counter + 1)


isPrime : Int -> Bool
isPrime number =
    let
        _ =
            Debug.log "The candidate is " number
    in
    isPrimeNumberFunc number 2


findNthPrimeNum : String -> Int -> Int -> String
findNthPrimeNum inputNumber candidate count =
    let
        number =
            Result.withDefault 0 (String.toInt inputNumber)

        _ =
            Debug.log "The prime is " (isPrime candidate)
    in
    if count < number && isPrime candidate then
        findNthPrimeNum inputNumber (candidate + 1) (count + 1)
    else if count < number && not (isPrime candidate) then
        findNthPrimeNum inputNumber (candidate + 1) count
    else
        (candidate - 1)
            |> toString


update : Msg -> Model -> Model
update msg model =
    case msg of
        UpdateQuery inputNumber ->
            { model | inputNumber = inputNumber }

        FindTheNthPrimeNumber inputNumber candidate count ->
            { model
                | result = findNthPrimeNum inputNumber candidate count
            }


view : Model -> Html Msg
view model =
    div [ class "main-container" ]
        [ header []
            [ h1 [] [ text "ELM Assignment 7" ]
            ]
        , table []
            [ tr []
                [ td [] [ text "Enter Number :" ]
                , td [] [ input [ class "inputCls", placeholder "Enter the number N", onInput UpdateQuery ] [] ]

                --    , td [] [ text <| "List is : " ++ model.inputList ]
                ]

            --, text <| "List is : " ++ model.inputList
            --  , br [] []
            --  , text "Enter Member"
            --  , input [ class "inputCls", placeholder "Enter element to search in List .. ", onInput UpdateMember ] []
            --  , br [] []
            ]
        , button [ class "srchBtn", onClick <| FindTheNthPrimeNumber model.inputNumber model.candidate model.count ] [ text "Search Element" ]
        , text (toString model.result)
        ]
