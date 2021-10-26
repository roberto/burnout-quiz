module Style exposing (..)

import Element exposing (rgb255)


colors :
    { primary : Element.Color
    , base : Element.Color
    , semantics : { highlight : Element.Color }
    , helper : Element.Color
    , other : Element.Color
    , neutrals : { lightGray : Element.Color }
    }
colors =
    { primary = rgb255 0xFA 0xBC 0x2A
    , base = rgb255 0xF2 0xED 0xEB
    , semantics =
        { highlight = rgb255 0xF0 0x53 0x65
        }
    , helper = rgb255 0xBD 0x93 0xBD
    , other = rgb255 0x92 0x5E 0x78
    , neutrals =
        { lightGray = rgb255 0xDD 0xDD 0xDD
        }
    }
