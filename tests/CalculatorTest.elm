module CalculatorTest exposing (..)

import Array
import Calculator
import Expect
import Fuzz exposing (int, list, maybe)
import Random
import Shrink
import String exposing (fromInt)
import Test exposing (..)


suite : Test
suite =
    let
        fuzzyQuestion =
            Fuzz.custom
                (Random.map2
                    (\selectedAnswer answersSize -> { selectedAnswer = Just selectedAnswer, answers = Array.initialize answersSize fromInt })
                    (Random.int -5 5)
                    (Random.int 0 10)
                )
                Shrink.noShrink

        answers =
            Array.fromList [ "a", "b", "c", "d" ]

        questionMaxValue =
            { selectedAnswer = Just 3, answers = answers }

        questionZeroValue =
            { selectedAnswer = Just 0, answers = answers }

        questionOtherValue =
            { selectedAnswer = Just 2, answers = answers }
    in
    describe "Calculator"
        [ describe "evaluateQuestion"
            [ test "first answer has the minimum value, zero" <|
                \_ ->
                    { selectedAnswer = Just 0, answers = answers }
                        |> Calculator.evaluateQuestion
                        |> Expect.within (Expect.Absolute 0.001) 0
            , test "second answer has one third" <|
                \_ ->
                    { selectedAnswer = Just 1, answers = answers }
                        |> Calculator.evaluateQuestion
                        |> Expect.within (Expect.Absolute 0.001) 0.333
            , test "third answer has two thirds" <|
                \_ ->
                    { selectedAnswer = Just 2, answers = answers }
                        |> Calculator.evaluateQuestion
                        |> Expect.within (Expect.Absolute 0.001) 0.666
            , test "last answer has the maximum value, one" <|
                \_ ->
                    { selectedAnswer = Just 3, answers = answers }
                        |> Calculator.evaluateQuestion
                        |> Expect.within (Expect.Absolute 0.001) 1
            , fuzz2 fuzzyQuestion (maybe int) "always return a Float between 0 and 1" <|
                \question fuzzyAnswer ->
                    { question | selectedAnswer = fuzzyAnswer }
                        |> Calculator.evaluateQuestion
                        |> Expect.all
                            [ Expect.atLeast 0
                            , Expect.atMost 1
                            ]
            ]
        , describe "evaluateQuestions"
            [ test "for just one with max value, returns max value" <|
                \_ ->
                    [ questionMaxValue ]
                        |> Calculator.evaluateQuestions
                        |> Expect.within (Expect.Absolute 0.001) 1
            , test "for just one with value zero, returns zero" <|
                \_ ->
                    [ questionZeroValue ]
                        |> Calculator.evaluateQuestions
                        |> Expect.within (Expect.Absolute 0.001) 0
            , test "for one with max and other with zero, it returns 0.5" <|
                \_ ->
                    [ questionZeroValue, questionMaxValue ]
                        |> Calculator.evaluateQuestions
                        |> Expect.within (Expect.Absolute 0.001) 0.5
            , test "for 0, 0.666 and 1, returns 0.888" <|
                \_ ->
                    [ questionMaxValue, questionMaxValue, questionOtherValue ]
                        |> Calculator.evaluateQuestions
                        |> Expect.within (Expect.Absolute 0.001) 0.888
            , fuzz (list fuzzyQuestion) "it always returns a Float between 0 and 1" <|
                \fuzzyQuestions ->
                    fuzzyQuestions
                        |> Calculator.evaluateQuestions
                        |> Expect.all
                            [ Expect.atLeast 0
                            , Expect.atMost 1
                            ]
            ]
        ]
