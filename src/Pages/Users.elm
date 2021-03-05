module Pages.Users exposing (Model, Msg, Params, page)

import Api.Data exposing (Data(..), expectJson)
import Api.Profile exposing (Profile, profilesDecoder)
import Html exposing (..)
import Html.Attributes exposing (class, height, href, placeholder, src, type_, value, width)
import Html.Events exposing (onClick, onInput)
import Http
import Server exposing (url)
import Shared
import Spa.Document exposing (Document)
import Spa.Page as Page exposing (Page)
import Spa.Url as Url exposing (Url)


page : Page Params Model Msg
page =
    Page.application
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        , save = save
        , load = load
        }



-- INIT


type alias Params =
    ()


type alias Model =
    { search : String
    , sorting : String
    , profiles : Data (List Profile)
    }


init : Shared.Model -> Url Params -> ( Model, Cmd Msg )
init shared { params } =
    ( { search = "", sorting = "lastname", profiles = Loading }, getUsers "" "lastname" { onResponse = ProfilesReceived } )



-- UPDATE


type Msg
    = ProfilesReceived (Data (List Profile))
    | Search String
    | ChangeSorting String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ProfilesReceived profiles ->
            ( { model | profiles = profiles }, Cmd.none )

        Search searching ->
            ( { model | search = searching }, getUsers searching model.sorting { onResponse = ProfilesReceived } )

        ChangeSorting sorting ->
            ( { model | sorting = sorting }, getUsers model.search sorting { onResponse = ProfilesReceived } )


save : Model -> Shared.Model -> Shared.Model
save model shared =
    shared


load : Shared.Model -> Model -> ( Model, Cmd Msg )
load shared model =
    ( model, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Model -> Document Msg
view model =
    { title = "Users | GoodFood"
    , body = [ viewProfiles model ]
    }


getUsers : String -> String -> { onResponse : Data (List Profile) -> Msg } -> Cmd Msg
getUsers searched sorting options =
    Http.get
        { url = url ++ "/users?_sort=" ++ sorting ++ "&_order=asc&q=" ++ searched
        , expect = Api.Data.expectJson options.onResponse profilesDecoder
        }


viewProfiles : Model -> Html Msg
viewProfiles model =
    case model.profiles of
        NotAsked ->
            text ""

        Loading ->
            div []
                [ img [ src "/assets/loading.gif" ] [] ]

        Success actualProfiles ->
            div []
                [ h1 [] [ text "Users" ]
                , button
                    [ case model.sorting of
                        "lastname" ->
                            class "active_category_button"

                        _ ->
                            class "category_button"
                    , onClick <| ChangeSorting "lastname"
                    ]
                    [ text "Search by last name" ]
                , button
                    [ case model.sorting of
                        "email" ->
                            class "active_category_button"

                        _ ->
                            class "category_button"
                    , onClick <| ChangeSorting "email"
                    ]
                    [ text "Search by email" ]
                , br [] []
                , input [ class "search_input", type_ "search", placeholder "Search...", onInput Search, value model.search ] []
                , div [ class "line_after_recipes" ] []
                , if List.isEmpty actualProfiles then
                    div [ class "profiles_list" ]
                        [ br [] []
                        , p [ class "err" ] [ text "No profiles yet..." ]
                        ]

                  else
                    div [ class "profiles_list" ]
                        (List.map viewProfile actualProfiles)
                ]

        Failure _ ->
            viewFetchError "Something went wrong!"


viewFetchError : String -> Html Msg
viewFetchError errorMessage =
    let
        errorHeading =
            "Couldn't fetch recipes."
    in
    div []
        [ h1 [] [ text errorHeading ]
        , text ("Error: " ++ errorMessage)
        ]


viewProfile : Profile -> Html Msg
viewProfile profile =
    ul [ class "profile_list" ]
        [ a [ href ("/profile/" ++ String.fromInt profile.id), class "link__name" ] [ text (profile.firstname ++ " " ++ profile.lastname) ]
        , br [] []
        , a [ href ("/profile/" ++ String.fromInt profile.id) ] [ img [ class "profile__image_users", src profile.image, width 100, height 100 ] [] ]
        , a [ href ("/profile/" ++ String.fromInt profile.id) ] [ p [ class "value" ] [ text profile.email ] ]
        , div [ class "line_after_recipes" ] []
        ]
