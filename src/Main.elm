module Main exposing (..)

import Array exposing (Array, length)
import Browser
import Debug exposing (toString)
import Element exposing (Element, alignLeft, alignRight, alpha, centerX, centerY, column, el, fill, layout, none, padding, paragraph, row, scrollbarY, text, width)
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


type Page
    = Intro
    | Questions
    | Result


type Section
    = Exhaustion
    | Cynicism


type alias Model =
    { questions : Array Question
    , currentQuestion : Int
    , page : Page
    }


type Msg
    = UserClickedOnAnwser Int
    | UserClickedOnBackButton
    | UserClickedOnNextButton
    | UserClickedOnStartButton


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
            , page = Intro
            }
        , update = update
        , view = view
        }


update : Msg -> Model -> Model
update msg model =
    case msg of
        UserClickedOnStartButton ->
            { model | page = Questions }

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
            if isFirstQuestion model then
                { model | page = Intro }

            else
                { model | currentQuestion = model.currentQuestion - 1 }

        UserClickedOnNextButton ->
            if isLastQuestion model then
                { model | page = Result }

            else
                { model | currentQuestion = model.currentQuestion + 1 }


updateQuestion : Question -> Int -> Question
updateQuestion (Question question) selectedAnswer =
    Question { question | selectedAnswer = Just selectedAnswer }


isFirstQuestion : Model -> Bool
isFirstQuestion model =
    model.currentQuestion == 0


isLastQuestion : Model -> Bool
isLastQuestion model =
    model.currentQuestion == (length model.questions - 1)


getCurrentQuestion : Model -> Maybe Question
getCurrentQuestion model =
    Array.get model.currentQuestion model.questions


view : Model -> Html Msg
view model =
    layout [] <|
        case model.page of
            Intro ->
                viewIntro

            Result ->
                viewResult model

            Questions ->
                viewQuestions model


viewIntro : Element Msg
viewIntro =
    row [ centerX, centerY ]
        [ button [ alignRight ] { onPress = Just <| UserClickedOnStartButton, label = text "Start" }
        ]


viewResult : Model -> Element msg
viewResult model =
    let
        calculateQuestion (Question { selectedAnswer, answers }) =
            toFloat (Maybe.withDefault 0 selectedAnswer) / toFloat (length answers - 1)

        calculateAverage total =
            total / toFloat (length model.questions)
    in
    model.questions
        |> Array.map calculateQuestion
        |> Array.foldr (+) 0
        |> calculateAverage
        |> (*) 100
        |> toString
        |> text


viewQuestions : Model -> Element Msg
viewQuestions model =
    row [ width fill ]
        [ el [ width fill ] none
        , column [ Element.Border.width 1, width fill, padding 5 ]
            [ viewQuestion <| getCurrentQuestion model
            , viewResult model
            , viewActions model
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


viewActions : Model -> Element Msg
viewActions model =
    row [ width fill ]
        [ backButton
        , nextButton model
        ]


backButton : Element Msg
backButton =
    button [ alignLeft ] { onPress = Just <| UserClickedOnBackButton, label = text "Back" }


nextButton : Model -> Element Msg
nextButton model =
    let
        selected =
            getCurrentQuestion model
                |> Maybe.andThen (\(Question { selectedAnswer }) -> selectedAnswer)
    in
    case selected of
        Just _ ->
            button [ alignRight ] { onPress = Just <| UserClickedOnNextButton, label = text "Next" }

        Nothing ->
            button [ alignRight, alpha 0.6 ] { onPress = Nothing, label = text "Next" }
