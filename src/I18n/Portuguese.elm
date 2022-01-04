module I18n.Portuguese exposing (..)

import Array
import FormatNumber exposing (format)
import FormatNumber.Locales exposing (Decimals(..), System(..), spanishLocale)
import I18n.Base
import Section exposing (Section(..))
import String exposing (fromInt)


answers : I18n.Base.Answers
answers =
    Array.fromList
        [ "Nunca"
        , "Algumas vezes ao ano ou menos"
        , "Uma vez ao mês ou menos"
        , "Algumas vezes por mês"
        , "Uma vez por semana"
        , "Algumas vezes por semana"
        , "Todo dia"
        ]


buttons : I18n.Base.ButtonsText
buttons =
    { start = "Iniciar"
    , next = "Avançar"
    , back = "Voltar"
    , finish = "Finalizar"
    }


questions : I18n.Base.QuestionsText
questions =
    { depersonalization =
        { first = "Estou mais duro e menos simpático com outras pessoas do que elas merecem."
        , second = "Estou preocupado como esse trabalho está me fazendo mais duro emocionalmente."
        }
    , selfInefficacy =
        { first = "Estou sentindo que estou alcançando menos do que poderia."
        , second = "Em minha opinião, estou ineficiente no meu trabalho."
        }
    , exhaustion =
        { first = "Encontro dificuldade para relaxar após um dia de trabalho."
        , second = "Depois de um dia de trabalho, me sinto atropelado e esgotado de energia física e mental."
        }
    , cynicism =
        { first = "Sinto cada vez menos conectado e engajado com o trabalho que faço."
        , second = "Não tenho clara idéia do valor e propósito do meu trabalho."
        }
    }


section : I18n.Base.SectionToString
section sec =
    case sec of
        Exhaustion ->
            "Exaustão"

        Depersonalization ->
            "Despersonalização"

        Cynicism ->
            "Cinismo"

        SelfInefficacy ->
            "Auto Ineficácia"


formatMaxResult : I18n.Base.FormatMaxResult
formatMaxResult max =
    " de " ++ fromInt max


formatEvaluation : I18n.Base.FormatEvaluation
formatEvaluation =
    format { spanishLocale | decimals = Max 1 }


t : I18n.Base.Text
t =
    { buttons = buttons
    , questions = questions
    , answers = answers
    , formatMaxResult = formatMaxResult
    , formatEvaluation = formatEvaluation
    , section = section
    }
