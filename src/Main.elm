module Main exposing (..)

import Array exposing (Array, length)
import Browser
import Debug exposing (toString)
import Element exposing (Element, alignLeft, alignRight, centerX, centerY, column, el, fill, focused, height, layout, minimum, mouseOver, moveDown, none, padding, paddingEach, paragraph, px, rgb255, row, spacing, text, textColumn, width)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input exposing (button)
import Html exposing (Html)
import Html.Attributes exposing (selected)
import Style exposing (colors)


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
    | Quiz
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
            , page = Quiz
            }
        , update = update
        , view = view
        }


update : Msg -> Model -> Model
update msg model =
    case msg of
        UserClickedOnStartButton ->
            { model | page = Quiz }

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
    layout [ Background.color colors.base ] <|
        case model.page of
            Intro ->
                viewIntro

            Result ->
                viewResult model

            Quiz ->
                viewQuiz model


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


viewQuiz : Model -> Element Msg
viewQuiz model =
    let
        currentQuestion =
            getCurrentQuestion model
    in
    row
        [ width fill
        , paddingEach { top = 30, bottom = 0, left = 0, right = 0 }
        ]
        [ el [ width fill ] none
        , column
            [ width fill
            , spacing 30
            ]
            [ viewQuestion currentQuestion
            , viewActions model
            ]
        , el [ width fill ] none
        ]


viewQuestion : Maybe Question -> Element Msg
viewQuestion maybeQuestion =
    case maybeQuestion of
        Just (Question { content, answers, selectedAnswer }) ->
            textColumn [ width fill, spacing 10 ]
                (paragraph
                    [ Border.widthEach { bottom = 1, top = 0, left = 0, right = 0 }
                    , width fill
                    , padding 12
                    , Element.height (fill |> minimum 64)
                    ]
                    [ text content ]
                    :: Array.toList (Array.indexedMap (viewAnswer selectedAnswer) answers)
                )

        Nothing ->
            text ""


viewAnswer : Maybe Int -> Int -> Answer -> Element Msg
viewAnswer maybeSelectedAnswer index answer =
    let
        selected =
            maybeSelectedAnswer
                |> Maybe.map (\selectedAnswer -> selectedAnswer == index)
                |> Maybe.withDefault False

        selectorBackground =
            Background.color <|
                if selected then
                    colors.primary

                else
                    colors.base

        selector =
            el
                [ width <| px 28
                , height <| px 28
                , centerY
                , padding 4
                , Border.rounded 6
                , Border.width 2
                , Border.color colors.primary
                ]
            <|
                el
                    [ width fill
                    , height fill
                    , Border.rounded 4
                    , selectorBackground
                    ]
                    none

        mouseOverStyle =
            [ Background.color colors.neutrals.lightGray ]
    in
    button []
        { onPress = Just <| UserClickedOnAnwser index
        , label =
            row
                [ spacing 10
                , mouseOver mouseOverStyle
                , focused mouseOverStyle
                , width fill
                , Border.rounded 6
                ]
                [ selector, text answer ]
        }


viewActions : Model -> Element Msg
viewActions model =
    row [ width fill ]
        [ backButton
        , nextButton model
        ]


backButton : Element Msg
backButton =
    let
        buttonStyle =
            [ alignLeft
            , padding 10
            , Font.variant Font.smallCaps
            , Font.color colors.semantics.highlight
            ]
    in
    button buttonStyle
        { onPress = Just <| UserClickedOnBackButton
        , label = text "Back"
        }


nextButton : Model -> Element Msg
nextButton model =
    let
        selected =
            getCurrentQuestion model
                |> Maybe.andThen (\(Question { selectedAnswer }) -> selectedAnswer)

        buttonStyleBase =
            [ padding 10
            , Font.variant Font.smallCaps
            , Border.rounded 6
            , alignRight
            , Font.color colors.base
            ]

        buttonStyleEnabled =
            buttonStyleBase
                ++ [ Font.color colors.other
                   , Background.color colors.primary
                   , Border.shadow
                        { blur = 0.5
                        , color = colors.other
                        , offset = ( 2, 2 )
                        , size = 1.7
                        }
                   , mouseOver
                        [ moveDown 1.2
                        , Border.shadow
                            { blur = 0.5
                            , color = colors.other
                            , offset = ( 1.5, 1.5 )
                            , size = 1
                            }
                        ]
                   ]

        buttonStyleDisabled =
            buttonStyleBase
                ++ [ Background.color colors.neutrals.lightGray
                   , Font.variant Font.smallCaps
                   , Border.shadow
                        { blur = 0.5
                        , color = rgb255 0xAA 0xAA 0xAA
                        , offset = ( 2, 2 )
                        , size = 1.7
                        }
                   ]
    in
    case selected of
        Just _ ->
            button buttonStyleEnabled { onPress = Just <| UserClickedOnNextButton, label = text "Next" }

        Nothing ->
            button
                buttonStyleDisabled
                { onPress = Nothing, label = text "Next" }
