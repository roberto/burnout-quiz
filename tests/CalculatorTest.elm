module CalculatorTest exposing (..)

import Array
import Calculator
import Expect
import Fuzz exposing (int, list, maybe, string)
import Test exposing (..)


suite : Test
suite =
    let
        answers =
            Array.fromList [ "a", "b", "c", "d" ]
    in
    describe "evaluateQuestion"
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
        , fuzz2 (maybe int) (list string) "always return a Float between 0 and 1" <|
            \fuzzyAnswer fuzzyList ->
                { selectedAnswer = fuzzyAnswer, answers = Array.fromList fuzzyList }
                    |> Calculator.evaluateQuestion
                    |> Expect.all
                        [ Expect.atLeast 0
                        , Expect.atMost 1
                        ]
        ]
