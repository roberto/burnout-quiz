module Calculator exposing (..)

import Array exposing (Array)


evaluateQuestion : { q | selectedAnswer : Maybe Int, answers : Array a } -> Float
evaluateQuestion { selectedAnswer, answers } =
    let
        answersMax =
            Array.length answers - 1

        answer =
            selectedAnswer
                |> Maybe.withDefault 0
                |> clamp 0 answersMax

        result =
            toFloat answer / toFloat answersMax
    in
    if isNaN result then
        0

    else
        result
