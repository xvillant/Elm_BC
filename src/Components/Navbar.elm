module Components.Navbar exposing (view)

import Api.User exposing (User)
import Html exposing (..)
import Html.Attributes exposing (alt, class, height, href, src, width)
import Html.Events exposing (onClick)
import Spa.Generated.Route as Route
import String
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
            viewHeaderNotLoggedIn options


viewHeaderLoggedIn :
    { user : Maybe User
    , url : Url
    , onSignOut : msg
    }
    -> Html msg
viewHeaderLoggedIn options =
    let
        myprofile =
            "/profile/"
                ++ String.fromInt
                    (case options.user of
                        Nothing ->
                            0

                        Just user ->
                            user.id
                    )
    in
    header []
        [ a [ href (Route.toString Route.Top) ] [ img [ class "logo", src "/assets/lunch.png", width 50, height 50, alt "logo" ] [] ]
        , ul [ class "nav__links" ]
            [ li []
                [ a
                    [ href (Route.toString Route.Top)
                    , case options.url.path of
                        "/" ->
                            class "active_link"

                        _ ->
                            class "navbar-elements"
                    ]
                    [ i [ class "fas fa-home" ] [] ]
                ]
            , li []
                [ a
                    [ href (Route.toString Route.Recipes)
                    , case options.url.path of
                        "/recipes" ->
                            class "active_link"

                        _ ->
                            class "navbar-elements"
                    ]
                    [ i [ class "fas fa-book" ] [] ]
                ]
            , li []
                [ a
                    [ href (Route.toString Route.New)
                    , case options.url.path of
                        "/new" ->
                            class "active_link"

                        _ ->
                            class "navbar-elements"
                    ]
                    [ i [ class "fas fa-plus-circle" ] [] ]
                ]
            , li []
                [ a
                    [ href myprofile
                    , if String.contains myprofile options.url.path then
                        class "active_link"

                      else
                        class "navbar-elements"
                    ]
                    [ i [ class "fas fa-user" ] [] ]
                ]
            , li []
                [ a
                    [ href (Route.toString Route.Settings)
                    , case options.url.path of
                        "/settings" ->
                            class "active_link"

                        _ ->
                            class "navbar-elements"
                    ]
                    [ i [ class "fas fa-cogs" ] [] ]
                ]
            , li [] [ a [ class "sign-out", onClick options.onSignOut ] [ i [ class "fas fa-sign-out-alt" ] [] ] ]
            ]
        ]


viewHeaderNotLoggedIn :
    { user : Maybe User
    , url : Url
    , onSignOut : msg
    }
    -> Html msg
viewHeaderNotLoggedIn options =
    header []
        [ a [ href (Route.toString Route.Top) ] [ img [ class "logo", src "/assets/lunch.png", width 50, height 50, alt "logo" ] [] ]
        , ul [ class "nav__links" ]
            [ li []
                [ a
                    [ href (Route.toString Route.Top)
                    , case options.url.path of
                        "/" ->
                            class "active_link"

                        _ ->
                            class "navbar-elements"
                    ]
                    [ i [ class "fas fa-home" ] [] ]
                ]
            , li []
                [ a
                    [ href (Route.toString Route.Login)
                    , case options.url.path of
                        "/login" ->
                            class "active_link"

                        _ ->
                            class "navbar-elements"
                    ]
                    [ text "sign in" ]
                ]
            , li []
                [ a
                    [ href (Route.toString Route.Register)
                    , case options.url.path of
                        "/register" ->
                            class "active_link"

                        _ ->
                            class "navbar-elements"
                    ]
                    [ text "sign up" ]
                ]
            ]
        ]
