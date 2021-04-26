module Pages.Profile.ProfileId_Int exposing (Model, Msg, Params, page)

import Api.Article exposing (Article, articlesDecoder)
import Api.Data exposing (Data(..), viewFetchError)
import Api.Profile exposing (Profile, profileDecoder)
import Api.User exposing (User)
import Browser.Navigation exposing (pushUrl)
import Components.TimeFormatting exposing (formatDate, formatTime)
import Html exposing (..)
import Html.Attributes exposing (class, height, href, src, width)
import Html.Events exposing (onClick)
import Http exposing (..)
import Server
import Shared
import Spa.Document exposing (Document)
import Spa.Page as Page exposing (Page)
import Spa.Url exposing (Url)
import Task
import Time


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
    , params : Params
    , user : Maybe User
    }


init : Shared.Model -> Url Params -> ( Model, Cmd Msg )
init shared { params } =
    case shared.user of
        Just user_ ->
            ( { profile = Loading
              , posts = Loading
              , warning = ""
              , zone = Time.utc
              , params = params
              , user = shared.user
              }
            , Cmd.batch [ getUserRequest user_.token params { onResponse = ReceivedUser }, getContentRequest user_.token params { onResponse = ReceivedPosts }, Task.perform TimeZone Time.here ]
            )

        Nothing ->
            ( { profile = Loading
              , posts = Loading
              , warning = ""
              , zone = Time.utc
              , params = params
              , user = Nothing
              }
            , pushUrl shared.key "/login"
            )



-- UPDATE


type Msg
    = ReceivedUser (Data Profile)
    | ReceivedPosts (Data (List Article))
    | TimeZone Time.Zone
    | DeleteArticle Int
    | DeleteResponse (Result Http.Error String)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ReceivedUser response ->
            ( { model | profile = response }, Cmd.none )

        ReceivedPosts posts ->
            ( { model | posts = posts }, Cmd.none )

        TimeZone tz ->
            ( { model | zone = tz }, Cmd.none )

        DeleteArticle articleid ->
            ( model
            , deleteArticle
                (case model.user of
                    Just u ->
                        u.token

                    Nothing ->
                        ""
                )
                articleid
            )

        DeleteResponse deleted ->
            case deleted of
                Ok value ->
                    ( model
                    , getContentRequest
                        (case model.user of
                            Just u ->
                                u.token

                            Nothing ->
                                ""
                        )
                        model.params
                        { onResponse = ReceivedPosts }
                    )

                Err _ ->
                    ( model, Cmd.none )


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
                , viewPosts model
                ]
            }

        _ ->
            { title = "Profile | GoodFood"
            , body =
                [ viewProfile model.zone model.profile
                , div [ class "warning_form" ]
                    [ text model.warning ]
                , viewPosts model
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
                [ div [] [ h1 [] [ text (value.firstname ++ " " ++ value.lastname) ] ]
                , div [] [ img [ class "profile__image", src value.image, width 150, height 150 ] [] ]
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


getContentRequest : String -> Params -> { onResponse : Data (List Article) -> Msg } -> Cmd Msg
getContentRequest tokenString params options =
    Http.request
        { method = "GET"
        , headers = [ Http.header "Authorization" ("Bearer " ++ tokenString) ]
        , url = Server.url ++ "/posts?userId=" ++ String.fromInt params.profileId ++ "&_sort=created&_order=desc"
        , body = Http.emptyBody
        , expect = Api.Data.expectJson options.onResponse articlesDecoder
        , timeout = Nothing
        , tracker = Nothing
        }


viewPosts : Model -> Html Msg
viewPosts model =
    case model.posts of
        Success actualPosts ->
            let
                userid =
                    List.repeat (List.length actualPosts)
                        (case model.user of
                            Just u ->
                                u.id

                            Nothing ->
                                0
                        )

                tzarray =
                    List.repeat (List.length actualPosts) model.zone
            in
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
                        (List.map3 viewPost userid actualPosts tzarray)
                ]

        _ ->
            text ""


getUserRequest : String -> Params -> { onResponse : Data Profile -> Msg } -> Cmd Msg
getUserRequest tokenString params options =
    Http.request
        { method = "GET"
        , headers = [ Http.header "Authorization" ("Bearer " ++ tokenString) ]
        , url = Server.url ++ "/users/" ++ String.fromInt params.profileId
        , body = Http.emptyBody
        , expect = Api.Data.expectJson options.onResponse profileDecoder
        , timeout = Nothing
        , tracker = Nothing
        }


viewPost : Int -> Article -> Time.Zone -> Html Msg
viewPost userid post tz =
    ul [ class "post_list" ]
        [ div [] [ a [ href ("/article/" ++ String.fromInt post.id) ] [ h2 [ class "post_name" ] [ text post.name ] ] ]
        , div []
            [ p [ class "datetime" ] [ text (formatDate tz post.created) ]
            , p [ class "datetime" ] [ text (formatTime tz post.created) ]
            ]
        , div [] [ img [ class "recipe__image", src post.image, width 500 ] [] ]
        , div []
            [ p [ class "title" ] [ text "ingredients" ]
            , div [ class "justify__content" ]
                [ li [ class "value" ] [ renderList post.ingredients ]
                ]
            ]
        , div []
            [ p [ class "title" ] [ text "recipe" ]
            , div [ class "justify__content__recipe" ]
                [ li [ class "value" ] [ text post.recipe ]
                ]
            ]
        , div []
            [ p [ class "title" ] [ text "duration" ]
            , li [ class "value" ]
                [ text <| String.fromInt post.duration ++ " minutes" ]
            ]
        , div []
            [ a [ href ("/article/" ++ String.fromInt post.id) ] [ button [ class "submit_button" ] [ text "Comment" ] ]
            , if post.userId == userid then
                button [ class "recipe_delete_button", onClick <| DeleteArticle post.id ] [ text "Delete" ]

              else
                text ""
            ]
        , div [ class "line_after_recipes" ] []
        ]


renderList : List String -> Html msg
renderList lst =
    ol [ class "ingredients__" ]
        (List.map (\l -> li [ class "value" ] [ text l ]) lst)


deleteArticle : String -> Int -> Cmd Msg
deleteArticle tokenString articleid =
    Http.request
        { method = "DELETE"
        , headers = [ Http.header "Authorization" ("Bearer " ++ tokenString) ]
        , url = Server.url ++ "/posts/" ++ String.fromInt articleid
        , body = Http.emptyBody
        , expect = expectString DeleteResponse
        , timeout = Nothing
        , tracker = Nothing
        }
