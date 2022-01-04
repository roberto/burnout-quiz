module I18n.Base exposing (..)

import I18n.English
import I18n.Portuguese
import I18n.Types exposing (Text)


type Language
    = English
    | Portuguese


for : Language -> Text
for language =
    case language of
        English ->
            I18n.English.t

        Portuguese ->
            I18n.Portuguese.t
