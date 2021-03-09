module Components.TimeFormatting exposing (formatDate, formatTime)

import DateFormat exposing (dayOfMonthSuffix, format, hourMilitaryNumber, minuteFixed, monthNameFull, text, yearNumber)
import Time exposing (Posix, Zone)


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
