module Main exposing (..)

import Array exposing (Array)
import Browser
import Calculator
import Element exposing (Element, alignLeft, alignRight, centerX, centerY, column, el, fill, focused, height, layout, minimum, mouseOver, moveDown, none, padding, paddingEach, paragraph, px, rgb255, row, shrink, spacing, text, textColumn, width)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font exposing (bold)
import Element.Input exposing (button)
import FormatNumber exposing (format)
import FormatNumber.Locales exposing (Decimals(..), System(..), usLocale)
import Html exposing (Html)
import Html.Attributes exposing (selected)
import ListIterator exposing (ListIterator, createListIterator, hasNext)
import String exposing (fromInt)
import Style exposing (colors)


type alias Answer =
    String


type alias Question =
    { content : String
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
    | SelfInefficacy
    | Depersonalization


type alias Model =
    { questions : ListIterator Question
    , page : Page
    }


type Msg
    = UserClickedOnAnwser Int
    | UserClickedOnBackButton
    | UserClickedOnNextButton
    | UserClickedOnStartButton


commonAnswers : Array Answer
commonAnswers =
    Array.fromList
        [ "Never"
        , "A few times a year or less"
        , "Once a month or less"
        , "A few times a month"
        , "Once a week"
        , "A few times a week"
        , "Every day"
        ]


initQuestions : ListIterator Question
initQuestions =
    createListIterator
        { content = "I find it difficult to relax after a day of work."
        , selectedAnswer = Nothing
        , section = Exhaustion
        }
        [ { content = "After a day of work, I feel run-down and drained of physical or emotional energy."
          , selectedAnswer = Nothing
          , section = Exhaustion
          }
        , { content = "I feel less and less connected and engaged with the work I do."
          , selectedAnswer = Nothing
          , section = Cynicism
          }
        , { content = "I do not have a clear idea of the value and purpose of my job."
          , selectedAnswer = Nothing
          , section = Cynicism
          }
        , { content = "I am harder and less sympathetic with people than perhaps they deserve."
          , selectedAnswer = Nothing
          , section = Depersonalization
          }
        , { content = "I am worried this job is making me harsher emotionally."
          , selectedAnswer = Nothing
          , section = Depersonalization
          }
        , { content = "I feel that I am achieving less than I should."
          , selectedAnswer = Nothing
          , section = SelfInefficacy
          }
        , { content = "In my opinion, I’m inefficient in my job."
          , selectedAnswer = Nothing
          , section = SelfInefficacy
          }
        ]


main : Program () Model Msg
main =
    Browser.sandbox
        { init =
            { questions = initQuestions
            , page = Intro
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
                question =
                    ListIterator.current model.questions
            in
            { model
                | questions =
                    model.questions
                        |> ListIterator.setCurrent (updateQuestion question answerIndex)
            }

        UserClickedOnBackButton ->
            if ListIterator.hasPrevious model.questions then
                { model | questions = ListIterator.previous model.questions }

            else
                { model | page = Intro }

        UserClickedOnNextButton ->
            if hasNext model.questions then
                { model | questions = ListIterator.next model.questions }

            else
                { model | page = Result }


updateQuestion : Question -> Int -> Question
updateQuestion question selectedAnswer =
    { question | selectedAnswer = Just selectedAnswer }


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
        [ button []
            { onPress = Just UserClickedOnStartButton
            , label = text "Start"
            }
        ]


viewResult : Model -> Element msg
viewResult model =
    let
        total =
            model.questions
                |> ListIterator.toList
                |> Calculator.evaluateQuestions

        maxResult =
            Array.length commonAnswers - 1
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
            [ row
                [ Border.rounded 100
                , backgroundGradient total
                , padding 30
                , width shrink
                , centerX
                ]
                [ viewTotalResult model
                , text <| " of " ++ fromInt maxResult
                ]
            , viewSectionResults model
            ]
        , el [ width fill ] none
        ]


viewTotalResult : Model -> Element msg
viewTotalResult model =
    let
        total =
            model.questions
                |> ListIterator.toList
                |> Calculator.evaluateQuestions
    in
    total
        |> formatEvaluation
        |> text
        |> el
            [ bold
            , Font.size 50
            ]


viewSectionResults : Model -> Element msg
viewSectionResults model =
    let
        sectionToSlider ( section, evaluation ) =
            [ row
                [ width fill
                , backgroundGradient evaluation
                , Border.rounded 6
                , padding 8
                ]
                [ text <|
                    sectionToString section
                , el [ alignRight ] (text <| formatEvaluation evaluation)
                ]
            ]

        sections =
            model.questions
                |> ListIterator.toList
                |> Calculator.evaluateQuestionsBySection
                |> List.concatMap sectionToSlider
    in
    column [ width fill, spacing 20 ] sections


formatEvaluation : Float -> String
formatEvaluation =
    format { usLocale | decimals = Max 1 }


backgroundGradient : Float -> Element.Attr decorative msg
backgroundGradient evaluation =
    Background.gradient
        { angle = pi / 2
        , steps =
            List.repeat (12 - round (evaluation * 2)) colors.primary
                ++ List.repeat (round (evaluation * 2)) colors.semantics.highlight
        }


sectionToString : Section -> String
sectionToString section =
    case section of
        Exhaustion ->
            "Exhaustion"

        Depersonalization ->
            "Depersonalization"

        Cynicism ->
            "Cynicism"

        SelfInefficacy ->
            "Self Inefficacy"


viewQuiz : Model -> Element Msg
viewQuiz model =
    let
        currentQuestion =
            ListIterator.current model.questions
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


viewQuestion : Question -> Element Msg
viewQuestion { content, selectedAnswer } =
    textColumn [ width fill, spacing 10 ]
        (paragraph
            [ Border.widthEach { bottom = 1, top = 0, left = 0, right = 0 }
            , width fill
            , padding 12
            , Element.height (fill |> minimum 64)
            ]
            [ text content ]
            :: Array.toList (Array.indexedMap (viewAnswer selectedAnswer) commonAnswers)
        )


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
            model.questions
                |> ListIterator.current
                |> .selectedAnswer

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

        label =
            if ListIterator.hasNext model.questions then
                "Next"

            else
                "Finish"
    in
    case selected of
        Just _ ->
            button buttonStyleEnabled
                { onPress = Just <| UserClickedOnNextButton
                , label = text label
                }

        Nothing ->
            button
                buttonStyleDisabled
                { onPress = Nothing, label = text "Next" }
