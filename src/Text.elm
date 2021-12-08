module Text exposing (answers, buttonBack, buttonFinish, buttonNext, buttonStart)

import Array exposing (Array)


answers : Array String
answers =
    Array.fromList
        [ "Never"
        , "A few times a year or less"
        , "Once a month or less"
        , "A few times a month"
        , "Once a week"
        , "A few times a week"
        , "Every day"
        ]


buttonStart : String
buttonStart =
    "Start"


buttonFinish : String
buttonFinish =
    "Finish"


buttonNext : String
buttonNext =
    "Next"


buttonBack : String
buttonBack =
    "Back"
