module Pages.Profile.ProfileId_Int exposing (Model, Msg, Params, page)

import Api.Data exposing (Data(..))
import Html exposing (..)
import Html.Attributes exposing (class, height, width, src, href)
import Http exposing (..)
import Json.Decode as D exposing (field)
import Server exposing (url)
import Shared
import Spa.Document exposing (Document)
import Spa.Page as Page exposing (Page)
import Spa.Url as Url exposing (Url)
import Api.Profile exposing (Profile, profileDecoder)
import Api.Article exposing (Article, articlesDecoder)
import TimeFormatting exposing (formatTime, formatDate)

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
    }

init : Shared.Model -> Url Params -> ( Model, Cmd Msg )
init shared { params } =
    ( { profile = Loading
      , posts = Loading 
      , warning = ""
      }
    , Cmd.batch[getUserRequest params { onResponse = ReceivedUser }, getContentRequest params {onResponse = ReceivedPosts}]
    )



-- UPDATE


type Msg
    = ReceivedUser (Data Profile)
    | ReceivedPosts (Data (List Article))


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ReceivedUser response ->
            ( { model | profile = response }, Cmd.none )
        ReceivedPosts posts ->
            ( { model | posts = posts }, Cmd.none )


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
            { title = "Profile | " ++ profile.email
            , body =
                [ viewProfile model.profile
                , div [ class "warning_form" ]
                    [ text model.warning ]
                , viewPosts model.posts
                ]
            }

        _ ->
            { title = "Profile"
            , body =
                [ viewProfile model.profile
                , div [ class "warning_form" ]
                    [ text model.warning ]
                , viewPosts model.posts
                ]
            }


viewProfile : Data Profile -> Html Msg
viewProfile profile =
    case profile of
        NotAsked ->
            text ""

        Loading ->
            div [ ]
                [ img [ src "/assets/loading.gif" ] [] ]

        Success value ->
            div [ ]
                [ h1 [ ] [ text (value.firstname ++ " " ++ value.lastname) ]
                , img [src value.image, width 80, height 80][]
                , div [ ]
                    [ p [ class "title" ] [ text "email" ]
                    , p [ class "value" ] [ text value.email ]
                    ]
                , div [ ]
                    [ p [ class "title" ] [ text "bio" ]
                    , p [ class "value" ] [ text value.bio ]
                    ]
                , div [ ]
                    [ p [ class "title" ] [ text "created at" ]
                    , p [ class "datetime" ] [ text (value.created |> formatDate)  ]
                    , p [ class "datetime" ] [ text (value.created |> formatTime)  ]
                    ]
                ]

        Failure _ ->
            viewFetchError "profile" "Something went wrong!"


getContentRequest : Params -> { onResponse : Data (List Article) -> Msg } -> Cmd Msg
getContentRequest params options =
    Http.get
        { url = Server.url ++ "/posts?profile.id=" ++ String.fromInt params.profileId ++ "&_sort=id&_order=desc"
        , expect = Api.Data.expectJson options.onResponse articlesDecoder
        }

viewPosts : Data (List Article) -> Html Msg
viewPosts posts =
    case posts of
        Success actualPosts ->
            div [ ]
                [ h2 [ ] [ text "My recipes" ]
                , div [ class "line_after_recipes" ] []
                , div [ class "articles_list" ]
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
    ul [ class "post_list" ]
        [ h2 [][text post.name]
        , p[class "datetime"][ text (formatDate <| post.created) ]
        , p[class "datetime"][ text (formatTime <| post.created) ]
        , p [ class "title" ] [ text "ingredients" ]
        , li [ class "value" ][ String.join ", " post.ingredients |> text ]
        , p [ class "title" ] [ text "recipe" ]
        , li [ class "value" ][ text post.recipe ]
        , br [] []
        , a [ href ("/article/" ++ String.fromInt post.id) ] [ button [ class "submit_button" ] [ text "Comment" ] ]
        , div [ class "line_after_recipes" ] []
        ]


viewFetchError : String -> String -> Html Msg
viewFetchError items errorMessage =
    let
        errorHeading =
            "Couldn't fetch " ++ items ++ "."
    in
    div [ ]
        [ h1 [ ] [ text errorHeading ]
        , text ("Error: " ++ errorMessage)
        ]
