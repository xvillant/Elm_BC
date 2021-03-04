module Components.TimeFormatting exposing (formatDate, formatTime)

import DateFormat exposing (dayOfMonthSuffix, text, monthNameFull, yearNumber, format, hourMilitaryNumber, minuteFixed)
import Time exposing (Zone, Posix)


formatDate : Zone -> Posix -> String
formatDate zone time =
    format
        [ dayOfMonthSuffix
        , text " "
        , monthNameFull
        , text ", "
        , yearNumber
        ]
        zone
        time


formatTime : Zone -> Posix -> String
formatTime zone time =
    format
        [ hourMilitaryNumber
        , text ":"
        , minuteFixed
        ]
        zone
        time
