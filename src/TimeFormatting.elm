module TimeFormatting exposing (formatDate, formatTime)

import DateFormat as DF
import Time

formatDate : Time.Posix -> String
formatDate =
    DF.format
        [ DF.dayOfMonthNumber
        , DF.text " "
        , DF.monthNameFull
        , DF.text ", "
        , DF.yearNumber
        ]
        Time.utc
formatTime : Time.Posix -> String
formatTime =
    DF.format
        [ DF.hourNumber
        , DF.text ":"
        , DF.minuteNumber
        ]
        Time.utc