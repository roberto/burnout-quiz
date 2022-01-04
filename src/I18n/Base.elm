module I18n.Base exposing (..)

import Array exposing (Array)
import Section exposing (Section(..))


type alias Answers =
    Array String


type alias ButtonsText =
    { start : String
    , next : String
    , back : String
    , finish : String
    }


type alias QuestionsText =
    { depersonalization : { first : String, second : String }
    , selfInefficacy : { first : String, second : String }
    , exhaustion : { first : String, second : String }
    , cynicism : { first : String, second : String }
    }


type alias SectionToString =
    Section -> String


type alias FormatMaxResult =
    Int -> String


type alias FormatEvaluation =
    Float -> String


type alias Text =
    { buttons : ButtonsText
    , questions : QuestionsText
    , answers : Array String
    , formatMaxResult : FormatMaxResult
    , formatEvaluation : FormatEvaluation
    , section : SectionToString
    }
