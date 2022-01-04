module I18n.English exposing (..)

import Array
import FormatNumber exposing (format)
import FormatNumber.Locales exposing (Decimals(..), System(..), usLocale)
import I18n.Base
import Section exposing (Section(..))
import String exposing (fromInt)


answers : I18n.Base.Answers
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


buttons : I18n.Base.ButtonsText
buttons =
    { start = "Start"
    , next = "Next"
    , back = "Back"
    , finish = "Finish"
    }


questions : I18n.Base.QuestionsText
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


section : I18n.Base.SectionToString
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


formatMaxResult : I18n.Base.FormatMaxResult
formatMaxResult max =
    " of " ++ fromInt max


formatEvaluation : I18n.Base.FormatEvaluation
formatEvaluation =
    format { usLocale | decimals = Max 1 }


t : I18n.Base.Text
t =
    { buttons = buttons
    , questions = questions
    , answers = answers
    , formatMaxResult = formatMaxResult
    , formatEvaluation = formatEvaluation
    , section = section
    }
