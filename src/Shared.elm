module Shared exposing
    ( Flags
    , Model
    , Msg
    , init
    , subscriptions
    , update
    , view
    )

import Browser.Navigation exposing (Key)
import Html exposing (..)
import Html.Attributes exposing (..)
import Spa.Document exposing (Document)
import Spa.Generated.Route as Route
import Url exposing (Url)



-- INIT


type alias Flags =
    ()


type alias Model =
    { url : Url
    , key : Key
    }


init : Flags -> Url -> Key -> ( Model, Cmd Msg )
init flags url key =
    ( Model url key
    , Cmd.none
    )



-- UPDATE


type Msg
    = ReplaceMe


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ReplaceMe ->
            ( model, Cmd.none )


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
        [ viewHeader
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


viewHeader : Html msg
viewHeader =
    header
        [ class "header" ]
        [ div
            [ class "inner-header" ]
            [ div
                [ class "logo-container" ]
                [ img [ src "/assets/recipelogo.svg", width 50 ] [], text "RECIPES" ]
            , ul
                [ class "navigation" ]
                [ a
                    [ class "link", href (Route.toString Route.Top) ]
                    [ li
                        [ class "navbar-elements" ]
                        [ text "home" ]
                    ]
                , a
                    [ class "link", href (Route.toString Route.Recipes) ]
                    [ li
                        [ class "navbar-elements" ]
                        [ text "recipes" ]
                    ]
                , a
                    [ class "link", href (Route.toString Route.Profile) ]
                    [ li
                        [ class "navbar-elements" ]
                        [ text "profile" ]
                    ]
                , a
                    [ class "link", href (Route.toString Route.Register) ]
                    [ li
                        [ class "navbar-elements" ]
                        [ text "sign up" ]
                    ]
                , a
                    [ class "link", href (Route.toString Route.Login) ]
                    [ li
                        [ class "navbar-elements" ]
                        [ text "sign in" ]
                    ]
                , a
                    [ class "link", href (Route.toString Route.Article) ]
                    [ li
                        [ class "navbar-elements" ]
                        [ text "article" ]
                    ]
                , a
                    [ class "link", href (Route.toString Route.New) ]
                    [ li
                        [ class "navbar-elements" ]
                        [ text "new article" ]
                    ]
                ]
            ]
        ]



--     a [ class "link", href (Route.toString Route.NotFound) ] [ text "Not found" ]
