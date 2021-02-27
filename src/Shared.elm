module Shared exposing
    ( Flags
    , Model
    , Msg
    , init
    , subscriptions
    , update
    , view
    )

import Api.User exposing (User, userDecoder)
import Browser.Navigation exposing (Key)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Json.Decode as D
import Ports exposing (clearUser, saveUser)
import Spa.Document exposing (Document)
import Spa.Generated.Route as Route
import Url exposing (Url)



-- INIT


type alias Flags =
    D.Value


type alias Model =
    { url : Url
    , key : Key
    , user : Maybe User
    }


init : Flags -> Url -> Key -> ( Model, Cmd Msg )
init json url key =
    let
        user =
            json
                |> D.decodeValue (D.field "user" userDecoder)
                |> Result.toMaybe
    in
    ( Model url key user
    , Cmd.none
    )



-- UPDATE


type Msg
    = SignOutSignal
    | SignInSignal User


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SignOutSignal ->
            ( { model | user = Nothing }, clearUser )

        SignInSignal user ->
            ( { model | user = Just user }, saveUser user )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view :
    { page : Document msg, toMsg : Msg -> msg }
    -> Model
    -> Document msg
view { page, toMsg } model =
    { title = page.title
    , body =
        [ viewHeader model
        , div [ class "page" ] page.body
        , viewFooter
        ]
    }


viewFooter : Html msg
viewFooter =
    footer
        [ class "footer" ]
        [ br [] []
        , text "This single-page application was created by Patrik Villant © 2021"
        , br [] []
        , br [] []
        , a [ style "margin-right" "10px", target "_blank", href "https://github.com/xvillant" ] [ i [ class "fab fa-github" ] [] ]
        , a [ target "_blank", href "https://www.facebook.com/patrik.villant10" ] [ i [ class "fab fa-facebook" ] [] ]
        ]


viewHeader : Model -> Html msg
viewHeader model =
    case model.user of
        Just user ->
            viewHeaderLoggedIn model user

        Nothing ->
            viewAll model


viewHeaderLoggedIn : Model -> User -> Html msg
viewHeaderLoggedIn model user =
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
                        [ case model.url.path of
                            "/" ->
                                class "active_link"

                            _ ->
                                class "navbar-elements"
                        ]
                        [ text "home" ]
                    ]
                , a
                    [ class "link", href (Route.toString Route.Recipes) ]
                    [ li
                        [ case model.url.path of
                            "/recipes" ->
                                class "active_link"

                            _ ->
                                class "navbar-elements"
                        ]
                        [ text "recipes" ]
                    ]
                , a
                    [ class "link", href "/profile/1" ]
                    [ li
                        [ case model.url.path of
                            "/profile" ->
                                class "active_link"

                            _ ->
                                class "navbar-elements"
                        ]
                        [ text "profile" ]
                    ]

                , a
                    [ class "link", href (Route.toString Route.Settings) ]
                    [ li
                        [ case model.url.path of
                            "/settings" ->
                                class "active_link"

                            _ ->
                                class "navbar-elements"
                        ]
                        [ text "settings" ]
                    ]
                , a
                    [ class "link", href (Route.toString Route.New) ]
                    [ li
                        [ case model.url.path of
                            "/new" ->
                                class "active_link"

                            _ ->
                                class "navbar-elements"
                        ]
                        [ text "new article" ]
                    ]
                , a
                    [ class "link"]
                    [ li
                        [ class "navbar-elements" ]
                        [ text "sign out" ]
                    ]
                ]
            ]
        ]


viewHeaderNotLoggedIn : Model -> Html msg
viewHeaderNotLoggedIn model =
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
                        [ case model.url.path of
                            "/" ->
                                class "active_link"

                            _ ->
                                class "navbar-elements"
                        ]
                        [ text "home" ]
                    ]
                , a
                    [ class "link", href (Route.toString Route.Register) ]
                    [ li
                        [ case model.url.path of
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
                        [ case model.url.path of
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



--     a [ class "link", href (Route.toString Route.NotFound) ] [ text "Not found" ]


viewAll : Model -> Html msg
viewAll model =
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
                        [ case model.url.path of
                            "/" ->
                                class "active_link"

                            _ ->
                                class "navbar-elements"
                        ]
                        [ i [ class "fas fa-home"] [] ]
                    ]
                , a
                    [ class "link", href (Route.toString Route.Recipes) ]
                    [ li
                        [ case model.url.path of
                            "/recipes" ->
                                class "active_link"

                            _ ->
                                class "navbar-elements"
                        ]
                        [ i [ class "fas fa-book"] []  ]
                    ]
                , a
                    [ class "link", href "/profile/1" ]
                    [ li
                        [ case model.url.path of
                            "/profile/1" ->
                                class "active_link"

                            _ ->
                                class "navbar-elements"
                        ]
                        [ i [ class "fas fa-user"] []  ]
                    ]
                , a
                    [ class "link", href (Route.toString Route.Settings) ]
                    [ li
                        [ case model.url.path of
                            "/settings" ->
                                class "active_link"

                            _ ->
                                class "navbar-elements"
                        ]
                        [ i [ class "fas fa-cogs"] []  ]
                    ]
                

                {--, a
                    [ class "link", href (Route.toString Route.Article) ]
                    [ li
                        [ case model.url.path of
                            "/article" ->
                                class "active_link"

                            _ ->
                                class "navbar-elements"
                        ]
                        [ text "article" ]
                    ]--}
                , a
                    [ class "link", href (Route.toString Route.New) ]
                    [ li
                        [ case model.url.path of
                            "/new" ->
                                class "active_link"

                            _ ->
                                class "navbar-elements"
                        ]
                        [ i [ class "fas fa-plus-circle"] [] ]
                    ]
                                , a
                    [ class "link", href (Route.toString Route.Register) ]
                    [ li
                        [ case model.url.path of
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
                        [ case model.url.path of
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
