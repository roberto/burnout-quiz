module Calculator exposing (evaluateQuestions, evaluateQuestionsBySection)

import AssocList


safeAverage : Int -> Int -> Float
safeAverage value total =
    let
        result =
            toFloat value / toFloat total
    in
    if isNaN result then
        0

    else
        result


evaluateQuestions : List { q | selectedAnswer : Maybe Int } -> Float
evaluateQuestions questions =
    let
        evaluateQuestion =
            .selectedAnswer
                >> Maybe.withDefault 0

        questionsSum =
            questions
                |> List.map evaluateQuestion
                |> List.sum

        questionsLength =
            List.length questions
    in
    safeAverage questionsSum questionsLength


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
