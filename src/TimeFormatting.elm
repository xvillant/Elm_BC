module TimeFormatting exposing (formatDate, formatTime)

import DateFormat as DF
import Time

formatDate : Time.Posix -> String
formatDate =
    DF.format
        [ DF.dayOfMonthSuffix
        , DF.text " "
        , DF.monthNameFull
        , DF.text ", "
        , DF.yearNumber
        ]
        Time.utc
formatTime : Time.Posix -> String
formatTime =
    DF.format
        [ DF.hourMilitaryFromOneFixed
        , DF.text ":"
        , DF.minuteFixed
        ]
        Time.utc