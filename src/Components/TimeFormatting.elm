module Components.TimeFormatting exposing (formatDate, formatTime)

import DateFormat as DF
import Time

formatDate : Time.Zone -> Time.Posix -> String
formatDate zone time=
    DF.format
        [ DF.dayOfMonthSuffix
        , DF.text " "
        , DF.monthNameFull
        , DF.text ", "
        , DF.yearNumber
        ]
        zone
        time
formatTime : Time.Zone -> Time.Posix -> String
formatTime zone time =
    DF.format
        [ DF.hourMilitaryNumber
        , DF.text ":"
        , DF.minuteFixed
        ]
        zone
        time