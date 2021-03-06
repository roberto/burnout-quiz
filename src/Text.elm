module Text exposing (t)

import Array exposing (Array)
import FormatNumber exposing (format)
import FormatNumber.Locales exposing (Decimals(..), System(..), usLocale)
import Section exposing (Section(..))
import String exposing (fromInt)


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


type alias ButtonsText =
    { start : String
    , next : String
    , back : String
    , finish : String
    }


buttons : ButtonsText
buttons =
    { start = "Start"
    , next = "Next"
    , back = "Back"
    , finish = "Finish"
    }


type alias QuestionsText =
    { depersonalization : { first : String, second : String }
    , selfInefficacy : { first : String, second : String }
    , exhaustion : { first : String, second : String }
    , cynicism : { first : String, second : String }
    }


questions : QuestionsText
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


section : Section -> String
section sec =
    case sec of
        Exhaustion ->
            "Exhaustion"

        Depersonalization ->
            "Depersonalization"

        Cynicism ->
            "Cynicism"

        SelfInefficacy ->
            "Self Inefficacy"


formatMaxResult : Int -> String
formatMaxResult max =
    " of " ++ fromInt max


formatEvaluation : Float -> String
formatEvaluation =
    format { usLocale | decimals = Max 1 }


type alias Text =
    { buttons : ButtonsText
    , questions : QuestionsText
    , answers : Array String
    , formatMaxResult : Int -> String
    , formatEvaluation : Float -> String
    , section : Section -> String
    }


t : Text
t =
    { buttons = buttons
    , questions = questions
    , answers = answers
    , formatMaxResult = formatMaxResult
    , formatEvaluation = formatEvaluation
    , section = section
    }
