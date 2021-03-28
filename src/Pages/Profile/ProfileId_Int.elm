module Pages.Profile.ProfileId_Int exposing (Model, Msg, Params, page)

import Api.Article exposing (Article, articlesDecoder)
import Api.Data exposing (Data(..), viewFetchError)
import Api.Profile exposing (Profile, profileDecoder)
import Browser.Navigation exposing (pushUrl)
import Components.TimeFormatting exposing (formatDate, formatTime)
import Html exposing (..)
import Html.Attributes exposing (class, height, href, src, width)
import Http exposing (..)
import Server
import Shared
import Spa.Document exposing (Document)
import Spa.Page as Page exposing (Page)
import Spa.Url exposing (Url)
import Task
import Time
import TimeZone exposing (europe__bratislava)


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
    { profileId : Int }


type alias Model =
    { profile : Data Profile
    , posts : Data (List Article)
    , warning : String
    , zone : Time.Zone
    }


init : Shared.Model -> Url Params -> ( Model, Cmd Msg )
init shared { params } =
    ( { profile = Loading
      , posts = Loading
      , warning = ""
      , zone = Time.utc
      }
    , case shared.user of
        Just user_ ->
            Cmd.batch [ getUserRequest params { onResponse = ReceivedUser }, getContentRequest params { onResponse = ReceivedPosts }, Task.perform Timezone Time.here ]

        Nothing ->
            pushUrl shared.key "/login"
    )



-- UPDATE


type Msg
    = ReceivedUser (Data Profile)
    | ReceivedPosts (Data (List Article))
    | Timezone Time.Zone


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ReceivedUser response ->
            ( { model | profile = response }, Cmd.none )

        ReceivedPosts posts ->
            ( { model | posts = posts }, Cmd.none )

        Timezone tz ->
            ( { model | zone = tz }, Cmd.none )


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
    case model.profile of
        Success profile ->
            { title = "Profile | " ++ profile.email ++ " | GoodFood"
            , body =
                [ viewProfile model.zone model.profile
                , div [ class "warning_form" ]
                    [ text model.warning ]
                , viewPosts model.posts
                ]
            }

        _ ->
            { title = "Profile | GoodFood"
            , body =
                [ viewProfile model.zone model.profile
                , div [ class "warning_form" ]
                    [ text model.warning ]
                , viewPosts model.posts
                ]
            }


viewProfile : Time.Zone -> Data Profile -> Html Msg
viewProfile tz profile =
    case profile of
        NotAsked ->
            text ""

        Loading ->
            div []
                [ img [ src "/assets/loading.gif" ] [] ]

        Success value ->
            div []
                [ h1 [] [ text (value.firstname ++ " " ++ value.lastname) ]
                , img [ class "profile__image", src value.image, width 150, height 150 ] []
                , div []
                    [ p [ class "title" ] [ text "email" ]
                    , p [ class "value" ] [ text value.email ]
                    ]
                , div []
                    [ p [ class "title" ] [ text "bio" ]
                    , p [ class "value" ] [ text value.bio ]
                    ]
                , div []
                    [ p [ class "title" ] [ text "registered at" ]
                    , p [ class "datetime" ] [ text (formatDate tz value.created) ]
                    , p [ class "datetime" ] [ text (formatTime tz value.created) ]
                    ]
                ]

        Failure failures ->
            viewFetchError "profile" failures


getContentRequest : Params -> { onResponse : Data (List Article) -> Msg } -> Cmd Msg
getContentRequest params options =
    Http.get
        { url = Server.url ++ "/posts?profile.id=" ++ String.fromInt params.profileId ++ "&_sort=created&_order=desc"
        , expect = Api.Data.expectJson options.onResponse articlesDecoder
        }


viewPosts : Data (List Article) -> Html Msg
viewPosts posts =
    case posts of
        Success actualPosts ->
            div []
                [ h2 [ class "my_recipes" ] [ text "My recipes" ]
                , div [ class "line_after_recipes" ] []
                , if List.isEmpty actualPosts then
                    div [ class "articles_list" ]
                        [ br [] []
                        , p [ class "err" ] [ text "No recipes yet..." ]
                        ]

                  else
                    div [ class "articles_list" ]
                        (List.map viewPost actualPosts)
                ]

        _ ->
            text ""


getUserRequest : Params -> { onResponse : Data Profile -> Msg } -> Cmd Msg
getUserRequest params options =
    Http.get
        { url = Server.url ++ "/users/" ++ String.fromInt params.profileId
        , expect = Api.Data.expectJson options.onResponse profileDecoder
        }


viewPost : Article -> Html Msg
viewPost post =
    let
        timezone =
            europe__bratislava ()
    in
    ul [ class "post_list" ]
        [ h2 [] [ text post.name ]
        , p [ class "datetime" ] [ text (formatDate timezone post.created) ]
        , p [ class "datetime" ] [ text (formatTime timezone post.created) ]
        , p [ class "title" ] [ text "ingredients" ]
        , div [ class "justify__content" ]
            [ li [ class "value" ] [ renderList post.ingredients ]
            ]
        , p [ class "title" ] [ text "recipe" ]
        , div [ class "justify__content__recipe" ]
            [ li [ class "value" ] [ text post.recipe ]
            ]
        , p [ class "title" ] [ text "duration" ]
        , li [ class "value" ]
            [ text <| String.fromInt post.duration ++ " minutes" ]
        , img [ class "recipe__image", src post.image, width 500 ] []
        , br [] []
        , a [ href ("/article/" ++ String.fromInt post.id) ] [ button [ class "submit_button" ] [ text "Comment" ] ]
        , div [ class "line_after_recipes" ] []
        ]

renderList : List String -> Html msg
renderList lst =
    ol [ class "ingredients__" ]
        (List.map (\l -> li [ class "value" ] [ text l ]) lst)
