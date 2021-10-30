module CalculatorTest exposing (..)

import Array
import Calculator
import Expect
import Fuzz exposing (int, list, maybe)
import Random
import Random.Array
import Random.Char
import Random.Extra
import Random.String
import Shrink
import String exposing (fromInt)
import Test exposing (..)


type Section
    = A
    | B
    | C


expectLength : Int -> List a -> Expect.Expectation
expectLength length list =
    Expect.equal length (List.length list)


expectListContent : (c -> a -> Expect.Expectation) -> List c -> List (List a -> Expect.Expectation)
expectListContent expectations expectedValues =
    let
        checkValue expectation index expected results =
            case Array.get index (Array.fromList results) of
                Just result ->
                    expectation expected result

                Nothing ->
                    Expect.fail <|
                        "Different number of expectations. "
                            ++ "Looking for expectation number "
                            ++ fromInt index
                            ++ " in results with length "
                            ++ fromInt (List.length results)
    in
    List.indexedMap (checkValue expectations) expectedValues


suite : Test
suite =
    let
        fuzzyQuestion =
            Fuzz.custom
                (Random.map3
                    (\selectedAnswer answersRandom section -> { selectedAnswer = selectedAnswer, answers = answersRandom, section = section })
                    (Random.Extra.maybe Random.Extra.bool <| Random.int -50 100)
                    (Random.int 0 10 |> Random.andThen (\len -> Random.Array.array len (Random.String.string 3 Random.Char.english)))
                    (Random.uniform A [ B, C ])
                )
                Shrink.noShrink

        answers =
            Array.fromList [ "a", "b", "c", "d" ]

        questionMaxValue =
            { selectedAnswer = Just 3, answers = answers, section = A }

        questionZeroValue =
            { selectedAnswer = Just 0, answers = answers, section = A }

        questionOtherValue =
            { selectedAnswer = Just 2, answers = answers, section = A }
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
        , let
            checkEvaluation ( _, expected ) ( _, returned ) =
                Expect.within (Expect.Absolute 0.001) expected returned

            checkSection ( expected, _ ) ( returned, _ ) =
                Expect.equal expected returned
          in
          describe "evaluateQuestionsBySection"
            [ test "for just one question, it returns its section and evaluation" <|
                \_ ->
                    let
                        expectedResult =
                            [ ( A, 0.666 ) ]
                    in
                    [ questionOtherValue ]
                        |> Calculator.evaluateQuestionsBySection
                        |> Expect.all
                            (expectLength (List.length expectedResult)
                                :: expectListContent checkEvaluation expectedResult
                                ++ expectListContent checkSection expectedResult
                            )
            , test "for just two questions from different sections, it returns their sections with evaluations" <|
                \_ ->
                    let
                        expectedResult =
                            [ ( B, 1 ), ( A, 0.666 ) ]
                    in
                    [ questionOtherValue, { questionMaxValue | section = B } ]
                        |> Calculator.evaluateQuestionsBySection
                        |> Expect.all
                            (expectLength (List.length expectedResult)
                                :: expectListContent checkEvaluation expectedResult
                                ++ expectListContent checkSection expectedResult
                            )
            , test "for just three questions from different sections, it returns their evaluations grouped by section" <|
                \_ ->
                    let
                        expectedResult =
                            [ ( B, 0.5 ), ( A, 0.666 ) ]
                    in
                    [ questionOtherValue, { questionMaxValue | section = B }, { questionZeroValue | section = B } ]
                        |> Calculator.evaluateQuestionsBySection
                        |> Expect.all
                            (expectLength (List.length expectedResult)
                                :: expectListContent checkEvaluation expectedResult
                                ++ expectListContent checkSection expectedResult
                            )
            , let
                checkValues index results =
                    case Array.get index (Array.fromList results) of
                        Just result ->
                            result
                                |> Tuple.second
                                |> Expect.all
                                    [ Expect.atLeast 0
                                    , Expect.atMost 1
                                    ]

                        Nothing ->
                            Expect.fail "error"
              in
              fuzz (list fuzzyQuestion) "it always returns Float between 0 and 1 as evaluations" <|
                \fuzzyQuestions ->
                    let
                        results =
                            fuzzyQuestions
                                |> Calculator.evaluateQuestionsBySection
                    in
                    if List.isEmpty results && List.isEmpty fuzzyQuestions then
                        Expect.pass

                    else
                        results
                            |> Expect.all
                                (List.map checkValues <| List.range 0 (List.length results - 1))
            ]
        ]
