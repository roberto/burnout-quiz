module FlowTest exposing (..)

import Main
import ProgramTest exposing (ProgramTest, clickButton, done, ensureViewHas, ensureViewHasNot)
import Test exposing (..)
import Test.Html.Selector exposing (all, containing, disabled, tag, text)


flow : Test
flow =
    describe "flow"
        [ test "Buttons Start, Next and Back behavior" <|
            \() ->
                start
                    |> step "Start button moves user to firt question"
                    |> clickButton "Start"
                    |> ensureViewHas [ text "I find it difficult to relax after a day of work" ]
                    |> step "Back button is always enabled"
                    |> clickButton "Back"
                    |> clickButton "Start"
                    |> step "Next button starts as disabled"
                    |> ensureViewHas [ all [ disabled True, text "Next" ] ]
                    |> step "Next button is enabled after answer selection"
                    |> clickButton "Every day"
                    |> ensureViewHasNot [ all [ disabled True, text "Next" ] ]
                    |> step "Next button moves user to second question"
                    |> clickButton "Next"
                    |> ensureViewHas [ text "After a day of work, I feel run-down and drained of physical or emotional energy." ]
                    |> step "...skipping to last question..."
                    |> clickButton "Never"
                    |> clickButton "Next"
                    |> clickButton "Never"
                    |> clickButton "Next"
                    |> clickButton "Never"
                    |> clickButton "Next"
                    |> clickButton "Never"
                    |> clickButton "Next"
                    |> clickButton "Never"
                    |> clickButton "Next"
                    |> clickButton "Never"
                    |> clickButton "Next"
                    |> step "Last question has no Next button"
                    |> ensureViewHasNot [ text "Next" ]
                    |> step "Last question has a Finish button"
                    |> ensureViewHas [ all [ disabled True, text "Finish" ] ]
                    |> step "Finish button is enabled after answer selection"
                    |> clickButton "Never"
                    |> clickButton "Finish"
                    |> step "Result page has no buttons"
                    |> ensureViewHasNot [ text "Back" ]
                    |> ensureViewHasNot [ text "Next" ]
                    |> ensureViewHasNot [ text "Finish" ]
                    |> done
        , test
            "Results calculation"
          <|
            \() ->
                start
                    |> clickButton "Start"
                    |> step "Exhaustion with last answers"
                    |> clickButton "Every day"
                    |> clickButton "Next"
                    |> clickButton "Every day"
                    |> clickButton "Next"
                    |> step "Cynicism with first answers"
                    |> clickButton "Never"
                    |> clickButton "Next"
                    |> clickButton "Never"
                    |> clickButton "Next"
                    |> step "Depersonalization with answers 2 and 3"
                    |> clickButton "Once a month or less"
                    |> clickButton "Next"
                    |> clickButton "A few times a month"
                    |> clickButton "Next"
                    |> step "Self Inefficacy with fourth answers"
                    |> clickButton "Once a week"
                    |> clickButton "Next"
                    |> clickButton "Once a week"
                    |> clickButton "Finish"
                    |> ensureViewHasSection "Exhaustion" "6"
                    |> ensureViewHasSection "Cynicism" "0"
                    |> ensureViewHasSection "Depersonalization" "2.5"
                    |> ensureViewHasSection "Self Inefficacy" "4"
                    |> done
        ]


start : ProgramTest Main.Model Main.Msg ()
start =
    ProgramTest.createSandbox
        { init = Main.init
        , update = Main.update
        , view = Main.view
        }
        |> ProgramTest.start ()


step : String -> a -> a
step _ =
    identity


ensureViewHasSection : String -> String -> ProgramTest model msg effect -> ProgramTest model msg effect
ensureViewHasSection section value =
    ensureViewHas
        [ tag "div"
        , containing
            [ tag "div"
            , containing
                [ tag "div"
                , containing
                    [ tag "div"
                    , containing
                        [ tag "div"
                        , containing [ text section ]
                        , containing [ text value ]
                        ]
                    ]
                ]
            ]
        ]
