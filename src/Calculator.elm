module Calculator exposing (..)

import AssocList


answersMaxValue : number
answersMaxValue =
    6


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
    let
        answer =
            selectedAnswer
                |> Maybe.withDefault 0
                |> clamp 0 answersMaxValue
                |> toFloat
    in
    safeAverage answer answersMaxValue


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
