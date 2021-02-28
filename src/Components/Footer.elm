module Components.Footer exposing (view)

import Html exposing (..)
import Html.Attributes exposing (class, style, target, href)


view : Html msg
view =
    footer
        [ class "footer" ]
        [ br [] []
        , text "This single-page application was created by Patrik Villant © 2021"
        , br [] []
        , br [] []
        , a [ style "margin-right" "10px", target "_blank", href "https://github.com/xvillant" ] [ i [ class "fab fa-github" ] [] ]
        , a [ target "_blank", href "https://www.facebook.com/patrik.villant10" ] [ i [ class "fab fa-facebook" ] [] ]
        ]