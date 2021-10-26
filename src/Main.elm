module Main exposing (..)

import Array exposing (Array)
import Browser
import Element exposing (Element, centerX, centerY, column, layout, row, text)
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
    = UserClickedAnwser Int


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
        UserClickedAnwser answerIndex ->
            let
                maybeQuestion =
                    Array.get model.currentQuestion model.questions
            in
            case maybeQuestion of
                Just question ->
                    { model
                        | questions = Array.set model.currentQuestion (updateQuestion question answerIndex) model.questions
                        , currentQuestion = min (model.currentQuestion + 1) (Array.length model.questions - 1)
                    }

                Nothing ->
                    model


updateQuestion : Question -> Int -> Question
updateQuestion (Question question) selectedAnswer =
    Question { question | selectedAnswer = Just selectedAnswer }


viewQuestion : Maybe Question -> Element.Element Msg
viewQuestion maybeQuestion =
    case maybeQuestion of
        Just (Question { content, answers }) ->
            column []
                (text content
                    :: Array.toList (Array.indexedMap viewAnswer answers)
                )

        Nothing ->
            text ""


viewAnswer : Int -> Answer -> Element Msg
viewAnswer index answer =
    button []
        { onPress = Just <| UserClickedAnwser index
        , label = text answer
        }


getCurrentQuestion : Model -> Maybe Question
getCurrentQuestion model =
    Array.get model.currentQuestion model.questions


view : Model -> Html Msg
view model =
    layout
        []
    <|
        row [ centerX, centerY ]
            [ viewQuestion <| getCurrentQuestion model
            ]
