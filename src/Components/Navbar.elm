module Components.Navbar exposing (view)

import Api.User exposing (User)
import Html exposing (..)
import Html.Attributes exposing (class, classList, height, href, src, width)
import Html.Events as Events
import Spa.Generated.Route as Route exposing (Route, toString)
import Url exposing (Url)


view :
    { user : Maybe User
    , url : Url
    , onSignOut : msg
    }
    -> Html msg
view options =
    case options.user of
        Just user -> 
            viewHeaderLoggedIn options
        Nothing ->
            viewAll options


viewHeaderLoggedIn :
    { user : Maybe User
    , url : Url
    , onSignOut : msg
    }
    -> Html msg
viewHeaderLoggedIn options =
    header
        [ class "header" ]
        [ div
            [ class "inner-header" ]
            [ div
                [ class "logo-container" ]
                [ img [ src "/assets/recipelogo.svg", width 50, height 50 ] [], text "RECIPES" ]
            , ul
                [ class "navigation" ]
                [ a
                    [ class "link", href (Route.toString Route.Top) ]
                    [ li
                        [ case options.url.path of
                            "/" ->
                                class "active_link"

                            _ ->
                                class "navbar-elements"
                        ]
                        [ i [ class "fas fa-home" ] [] ]
                    ]
                , a
                    [ class "link", href (Route.toString Route.Recipes) ]
                    [ li
                        [ case options.url.path of
                            "/recipes" ->
                                class "active_link"

                            _ ->
                                class "navbar-elements"
                        ]
                        [ i [ class "fas fa-book" ] [] ]
                    ]
                , a
                    [ class "link", href "/profile/1" ]
                    [ li
                        [ case options.url.path of
                            "/profile" ->
                                class "active_link"

                            _ ->
                                class "navbar-elements"
                        ]
                        [ i [ class "fas fa-user" ] [] ]
                    ]
                , a
                    [ class "link", href (Route.toString Route.Settings) ]
                    [ li
                        [ case options.url.path of
                            "/settings" ->
                                class "active_link"

                            _ ->
                                class "navbar-elements"
                        ]
                        [ i [ class "fas fa-cogs" ] [] ]
                    ]
                , a
                    [ class "link", href (Route.toString Route.New) ]
                    [ li
                        [ case options.url.path of
                            "/new" ->
                                class "active_link"

                            _ ->
                                class "navbar-elements"
                        ]
                        [ i [ class "fas fa-plus-circle" ] [] ]
                    ]
                , a
                    [ class "link", Events.onClick options.onSignOut ]
                    [ li
                        [ class "navbar-elements" ]
                        [ i [ class "fas fa-sign-out-alt" ] [] ]
                    ]
                ]
            ]
        ]


viewHeaderNotLoggedIn :
    { user : Maybe User
    , url : Url
    , onSignOut : msg
    }
    -> Html msg
viewHeaderNotLoggedIn options =
    header
        [ class "header" ]
        [ div
            [ class "inner-header" ]
            [ div
                [ class "logo-container" ]
                [ img [ src "/assets/recipelogo.svg", width 50, height 50 ] [], text "RECIPES" ]
            , ul
                [ class "navigation" ]
                [ a
                    [ class "link", href (Route.toString Route.Top) ]
                    [ li
                        [ case options.url.path of
                            "/" ->
                                class "active_link"

                            _ ->
                                class "navbar-elements"
                        ]
                        [ i [ class "fas fa-home" ] [] ]
                    ]
                , a
                    [ class "link", href (Route.toString Route.Register) ]
                    [ li
                        [ case options.url.path of
                            "/register" ->
                                class "active_link"

                            _ ->
                                class "navbar-elements"
                        ]
                        [ text "sign up" ]
                    ]
                , a
                    [ class "link", href (Route.toString Route.Login) ]
                    [ li
                        [ case options.url.path of
                            "/login" ->
                                class "active_link"

                            _ ->
                                class "navbar-elements"
                        ]
                        [ text "sign in" ]
                    ]
                ]
            ]
        ]


viewAll :
    { user : Maybe User
    , url : Url
    , onSignOut : msg
    }
    -> Html msg
viewAll options =
    header
        [ class "header" ]
        [ div
            [ class "inner-header" ]
            [ div
                [ class "logo-container" ]
                [ img [ src "/assets/recipelogo.svg", width 50, height 50 ] [], text "RECIPES" ]
            , ul
                [ class "navigation" ]
                [ a
                    [ class "link", href (Route.toString Route.Top) ]
                    [ li
                        [ case options.url.path of
                            "/" ->
                                class "active_link"

                            _ ->
                                class "navbar-elements"
                        ]
                        [ i [ class "fas fa-home" ] [] ]
                    ]
                , a
                    [ class "link", href (Route.toString Route.Recipes) ]
                    [ li
                        [ case options.url.path of
                            "/recipes" ->
                                class "active_link"

                            _ ->
                                class "navbar-elements"
                        ]
                        [ i [ class "fas fa-book" ] [] ]
                    ]
                , a
                    [ class "link", href "/profile/1" ]
                    [ li
                        [ case options.url.path of
                            "/profile/1" ->
                                class "active_link"

                            _ ->
                                class "navbar-elements"
                        ]
                        [ i [ class "fas fa-user" ] [] ]
                    ]
                , a
                    [ class "link", href (Route.toString Route.Settings) ]
                    [ li
                        [ case options.url.path of
                            "/settings" ->
                                class "active_link"

                            _ ->
                                class "navbar-elements"
                        ]
                        [ i [ class "fas fa-cogs" ] [] ]
                    ]
                , a
                    [ class "link", href (Route.toString Route.New) ]
                    [ li
                        [ case options.url.path of
                            "/new" ->
                                class "active_link"

                            _ ->
                                class "navbar-elements"
                        ]
                        [ i [ class "fas fa-plus-circle" ] [] ]
                    ]
                , a
                    [ class "link", Events.onClick options.onSignOut ]
                    [ li
                        [ class "navbar-elements" ]
                        [ i [ class "fas fa-sign-out-alt" ] [] ]
                    ]
                , a
                    [ class "link", href (Route.toString Route.Register) ]
                    [ li
                        [ case options.url.path of
                            "/register" ->
                                class "active_link"

                            _ ->
                                class "navbar-elements"
                        ]
                        [ text "sign up" ]
                    ]
                , a
                    [ class "link", href (Route.toString Route.Login) ]
                    [ li
                        [ case options.url.path of
                            "/login" ->
                                class "active_link"

                            _ ->
                                class "navbar-elements"
                        ]
                        [ text "sign in" ]
                    ]
                ]
            ]
        ]