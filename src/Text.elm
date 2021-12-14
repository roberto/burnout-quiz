module Text exposing (answers, buttonBack, buttonFinish, buttonNext, buttonStart, questions, sectionToString)

import Array exposing (Array)
import Section exposing (Section(..))


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


questions :
    { depersonalization : { first : String, second : String }
    , selfInefficacy : { first : String, second : String }
    , exhaustion : { first : String, second : String }
    , cynicism : { first : String, second : String }
    }
questions =
    { depersonalization =
        { first = "I am harder and less sympathetic with people than perhaps they deserve."
        , second = "I am worried this job is making me harsher emotionally."
        }
    , selfInefficacy =
        { first = "I feel that I am achieving less than I should."
        , second = "In my opinion, I’m inefficient in my job."
        }
    , exhaustion =
        { first = "I find it difficult to relax after a day of work."
        , second = "After a day of work, I feel run-down and drained of physical or emotional energy."
        }
    , cynicism =
        { first = "I feel less and less connected and engaged with the work I do."
        , second = "I do not have a clear idea of the value and purpose of my job."
        }
    }


sectionToString : Section -> String
sectionToString section =
    case section of
        Exhaustion ->
            "Exhaustion"

        Depersonalization ->
            "Depersonalization"

        Cynicism ->
            "Cynicism"

        SelfInefficacy ->
            "Self Inefficacy"
