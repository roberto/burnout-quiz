module Calculator exposing (..)

import Array exposing (Array)


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


evaluateQuestion : { q | selectedAnswer : Maybe Int, answers : Array a } -> Float
evaluateQuestion { selectedAnswer, answers } =
    let
        answersMax =
            answers
                |> Array.length
                |> (\v -> v - 1)

        answer =
            selectedAnswer
                |> Maybe.withDefault 0
                |> clamp 0 answersMax
                |> toFloat
    in
    safeAverage answer answersMax


evaluateQuestions : List { q | selectedAnswer : Maybe Int, answers : Array a } -> Float
evaluateQuestions questions =
    let
        questionsSum =
            questions
                |> List.map evaluateQuestion
                |> List.sum
    in
    safeAverage questionsSum <| List.length questions
