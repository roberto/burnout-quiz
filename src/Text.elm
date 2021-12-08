module Text exposing (answers)

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
