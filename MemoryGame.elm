module Main exposing (..)

import Browser
import Html exposing (Html, div, button, text, span)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)
import Random
import List.Extra exposing (getAt)

-- MODEL

type alias Model =
    { cards : List Card
    , flippedIndices : List Int
    , matchedIndices : List Int
    , gameOver : Bool
    , attempts : Int
    }

type alias Card =
    { value : String, faceUp : Bool }

init : Model
init =
    { cards = generateCards
    , flippedIndices = []
    , matchedIndices = []
    , gameOver = False
    , attempts = 0
    }

-- GENERATE CARDS

generateCards : List Card
generateCards =
    let
        values = ["A", "B", "C", "D", "E", "F"]
        doubled = values ++ values
    in
    List.map (\value -> { value = value, faceUp = False }) (shuffle doubled)

shuffle : List a -> List a
shuffle list =
    case Random.shuffle list of
        Ok shuffled -> shuffled
        Err _ -> list

-- UPDATE

type Msg
    = FlipCard Int
    | ResetGame

update : Msg -> Model -> Model
update msg model =
    case msg of
        FlipCard index ->
            if model.gameOver then
                model
            else
                flipCard index model

        ResetGame ->
            init

flipCard : Int -> Model -> Model
flipCard index model =
    if List.length model.flippedIndices == 2 then
        checkMatch model
    else
        { model | flippedIndices = index :: model.flippedIndices }

checkMatch : Model -> Model
checkMatch model =
    let
        [firstIndex, secondIndex] = model.flippedIndices
        firstCard = getAt firstIndex model.cards |> Maybe.withDefault { value = "", faceUp = False }
        secondCard = getAt secondIndex model.cards |> Maybe.withDefault { value = "", faceUp = False }
    in
    if firstCard.value == secondCard.value then
        { model |
            cards = updateCardState firstIndex True (updateCardState secondIndex True model.cards)
            , flippedIndices = []
            , matchedIndices = firstIndex :: secondIndex :: model.matchedIndices
            , gameOver = checkGameOver model.cards
            , attempts = model.attempts + 1
        }
    else
        { model |
            cards = updateCardState firstIndex False (updateCardState secondIndex False model.cards)
            , flippedIndices = []
            , attempts = model.attempts + 1
        }

updateCardState : Int -> Bool -> List Card -> List Card
updateCardState index state cards =
    List.indexedMap (\i card -> if i == index then { card | faceUp = state } else card) cards

checkGameOver : List Card -> Bool
checkGameOver cards =
    List.all (\card -> card.faceUp) cards

-- VIEW

view : Model -> Html Msg
view model =
    div [ style "font-family" "Arial, sans-serif", style "text-align" "center" ]
        [ header model
        , div [ style "display" "grid", style "grid-template-columns" "repeat(4, 100px)", style "gap" "10px", style "margin-top" "20px" ]
            (List.indexedMap (cardView model) model.cards)
        , if model.gameOver then
            div [ style "margin-top" "20px" ]
                [ button [ onClick ResetGame ] [ text "Play Again" ] ]
          else
            text ""
        ]

header : Model -> Html Msg
header model =
    div [ style "margin-bottom" "20px" ]
        [ text ("Attempts: " ++ String.fromInt model.attempts)
        , if model.gameOver then
            div []
                [ text "You Win!" ]
          else
            text ""
        ]

cardView : Model -> Int -> Card -> Html Msg
cardView model index card =
    let
        isFlipped = List.member index model.flippedIndices || List.member index model.matchedIndices
    in
    div [ onClick (FlipCard index), style "display" "inline-block", style "width" "80px", style "height" "80px", style "background-color" "#ececec", style "text-align" "center", style "line-height" "80px", style "border-radius" "10px", style "cursor" "pointer", style "box-shadow" "2px 2px 10px rgba(0, 0, 0, 0.1)" ]
        [ if isFlipped then
            div [ style "font-size" "24px", style "color" "#333" ] [ text card.value ]
          else
            div [ style "font-size" "24px", style "color" "#aaa" ] [ text "?" ]
        ]

-- MAIN

main =
    Browser.sandbox { init = init, update = update, view = view }
