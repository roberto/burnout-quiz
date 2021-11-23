module CalculatorTest exposing (..)

import Array
import Calculator
import Expect
import Fuzz exposing (list)
import Random
import Random.Extra
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
                (Random.map2
                    (\selectedAnswer section -> { selectedAnswer = selectedAnswer, section = section })
                    (Random.Extra.maybe Random.Extra.bool <| Random.int 0 6)
                    (Random.uniform A [ B, C ])
                )
                Shrink.noShrink

        questionMaxValue =
            { selectedAnswer = Just 6, section = A }

        questionZeroValue =
            { selectedAnswer = Just 0, section = A }

        questionOtherValue =
            { selectedAnswer = Just 2, section = A }
    in
    describe "Calculator"
        [ describe "evaluateQuestions"
            [ test "for just one with max value, returns max value" <|
                \_ ->
                    [ questionMaxValue ]
                        |> Calculator.evaluateQuestions
                        |> Expect.within (Expect.Absolute 0.001) 6
            , test "for just one with value zero, returns zero" <|
                \_ ->
                    [ questionZeroValue ]
                        |> Calculator.evaluateQuestions
                        |> Expect.within (Expect.Absolute 0.001) 0
            , test "for one with max and other with zero, it returns 3" <|
                \_ ->
                    [ questionZeroValue, questionMaxValue ]
                        |> Calculator.evaluateQuestions
                        |> Expect.within (Expect.Absolute 0.001) 3
            , test "for 0, 0.333 and 1, returns 0.888" <|
                \_ ->
                    [ questionZeroValue, questionMaxValue, questionOtherValue ]
                        |> Calculator.evaluateQuestions
                        |> Expect.within (Expect.Absolute 0.001) 2.667
            , fuzz (list fuzzyQuestion) "it always returns a Float between 0 and 1" <|
                \fuzzyQuestions ->
                    fuzzyQuestions
                        |> Calculator.evaluateQuestions
                        |> Expect.all
                            [ Expect.atLeast 0
                            , Expect.atMost 6
                            ]
            ]
        , let
            checkEvaluation ( _, expected ) ( _, returned ) =
                Expect.within (Expect.Absolute 0.001) expected returned

            checkSection ( expected, _ ) ( returned, _ ) =
                Expect.equal expected returned
          in
          describe "evaluateQuestionsBySection"
            [ test "for one question, it returns its section and evaluation" <|
                \_ ->
                    let
                        expectedResult =
                            [ ( A, 2 ) ]
                    in
                    [ questionOtherValue ]
                        |> Calculator.evaluateQuestionsBySection
                        |> Expect.all
                            (expectLength (List.length expectedResult)
                                :: expectListContent checkEvaluation expectedResult
                                ++ expectListContent checkSection expectedResult
                            )
            , test "for two questions from different sections, it returns their sections with evaluations" <|
                \_ ->
                    let
                        expectedResult =
                            [ ( B, 6 ), ( A, 2 ) ]
                    in
                    [ questionOtherValue, { questionMaxValue | section = B } ]
                        |> Calculator.evaluateQuestionsBySection
                        |> Expect.all
                            (expectLength (List.length expectedResult)
                                :: expectListContent checkEvaluation expectedResult
                                ++ expectListContent checkSection expectedResult
                            )
            , test "for three questions from different sections, it returns their evaluations grouped by section" <|
                \_ ->
                    let
                        expectedResult =
                            [ ( B, 3 ), ( A, 2 ) ]
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
                                    , Expect.atMost 6
                                    ]

                        Nothing ->
                            Expect.fail "error"
              in
              fuzz (list fuzzyQuestion) "it always returns Float between 0 and 6 as evaluations" <|
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
