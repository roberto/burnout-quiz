module Calculator exposing (evaluateQuestion, evaluateQuestions, evaluateQuestionsBySection)

import AssocList


answersMaxValue : number
answersMaxValue =
    6


answerMultiplier : Float
answerMultiplier =
    1 / answersMaxValue


safeAverage : Float -> Int -> Float
safeAverage value total =
    let
        result =
            value / toFloat total
    in
    if isNaN result then
        0

    else
        result


evaluateQuestion : { q | selectedAnswer : Maybe Int } -> Float
evaluateQuestion { selectedAnswer } =
    selectedAnswer
        |> Maybe.withDefault 0
        |> clamp 0 answersMaxValue
        |> toFloat
        |> (*) answerMultiplier


evaluateQuestions : List { q | selectedAnswer : Maybe Int } -> Float
evaluateQuestions questions =
    let
        questionsSum =
            questions
                |> List.map evaluateQuestion
                |> List.sum
    in
    safeAverage questionsSum <| List.length questions


evaluateQuestionsBySection : List { q | selectedAnswer : Maybe Int, section : b } -> List ( b, Float )
evaluateQuestionsBySection questions =
    let
        upsert dict key value =
            AssocList.get key dict
                |> Maybe.withDefault []
                |> (\list -> AssocList.insert key (value :: list) dict)
    in
    questions
        |> List.foldl
            (\question dict -> upsert dict question.section question)
            AssocList.empty
        |> AssocList.map (\_ -> evaluateQuestions)
        |> AssocList.toList
