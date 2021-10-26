module Main exposing (..)

import Array exposing (Array)
import Browser
import Element exposing (Element, alignLeft, alignRight, column, el, fill, layout, none, padding, paragraph, row, scrollbarY, text, width)
import Element.Border
import Element.Input exposing (button)
import Html exposing (Html)


type alias Answer =
    String


type Question
    = Question
        { content : String
        , answers : Array Answer
        , selectedAnswer : Maybe Int
        , section : Section
        }


type Section
    = Exhaustion
    | Cynicism


type alias Model =
    { questions : Array Question
    , currentQuestion : Int
    }


type Msg
    = UserClickedOnAnwser Int
    | UserClickedOnBackButton
    | UserClickedOnNextButton


periodAnswers : Array Answer
periodAnswers =
    Array.fromList [ "Never", "A few times a year or less", "Once a month or less", "A few times a month", "Once a week", "A few times a week", "Every day" ]


initQuestions : Array Question
initQuestions =
    Array.fromList
        [ Question
            { content = "I find it difficult to relax after a day of work."
            , answers = periodAnswers
            , selectedAnswer = Nothing
            , section = Exhaustion
            }
        , Question
            { content = "After a day of work, I feel run-down and drained of physical or emotional energy."
            , answers = periodAnswers
            , selectedAnswer = Nothing
            , section = Exhaustion
            }
        , Question
            { content = "I feel less and less connected and engaged with the work I do."
            , answers = periodAnswers
            , selectedAnswer = Nothing
            , section = Cynicism
            }
        , Question
            { content = "I do not have a clear idea of the value and purpose of my job."
            , answers = periodAnswers
            , selectedAnswer = Nothing
            , section = Cynicism
            }
        ]


main : Program () Model Msg
main =
    Browser.sandbox
        { init =
            { questions = initQuestions
            , currentQuestion = 0
            }
        , update = update
        , view = view
        }


update : Msg -> Model -> Model
update msg model =
    case msg of
        UserClickedOnAnwser answerIndex ->
            let
                maybeQuestion =
                    Array.get model.currentQuestion model.questions
            in
            case maybeQuestion of
                Just question ->
                    { model
                        | questions = Array.set model.currentQuestion (updateQuestion question answerIndex) model.questions
                    }

                Nothing ->
                    model

        UserClickedOnBackButton ->
            { model | currentQuestion = max (model.currentQuestion - 1) 0 }

        UserClickedOnNextButton ->
            { model | currentQuestion = min (model.currentQuestion + 1) (Array.length model.questions - 1) }


updateQuestion : Question -> Int -> Question
updateQuestion (Question question) selectedAnswer =
    Question { question | selectedAnswer = Just selectedAnswer }


view : Model -> Html Msg
view model =
    layout
        []
    <|
        row [ width fill ]
            [ el [ width fill ] none
            , column [ Element.Border.width 1, width fill, padding 5 ]
                [ viewQuestion <| getCurrentQuestion model
                , viewActions
                ]
            , el [ width fill, scrollbarY ] none
            ]


viewQuestion : Maybe Question -> Element.Element Msg
viewQuestion maybeQuestion =
    case maybeQuestion of
        Just (Question { content, answers, selectedAnswer }) ->
            column []
                (paragraph [] [ text content ]
                    :: Array.toList (Array.indexedMap (viewAnswer selectedAnswer) answers)
                )

        Nothing ->
            text ""


viewAnswer : Maybe Int -> Int -> Answer -> Element Msg
viewAnswer maybeSelectedAnswer index answer =
    let
        normalButton =
            button []
                { onPress = Just <| UserClickedOnAnwser index
                , label = text answer
                }

        selectedButton =
            button []
                { onPress = Just <| UserClickedOnAnwser index
                , label = text <| answer ++ "   <----"
                }
    in
    case maybeSelectedAnswer of
        Just selectedAnswer ->
            if selectedAnswer == index then
                selectedButton

            else
                normalButton

        Nothing ->
            normalButton


getCurrentQuestion : Model -> Maybe Question
getCurrentQuestion model =
    Array.get model.currentQuestion model.questions


viewActions : Element Msg
viewActions =
    row [ width fill ]
        [ button [ alignLeft ] { onPress = Just <| UserClickedOnBackButton, label = text "Back" }
        , button [ alignRight ] { onPress = Just <| UserClickedOnNextButton, label = text "Next" }
        ]
