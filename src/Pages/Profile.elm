module Pages.Profile exposing (Model, Msg, Params, page)

import Api.Data exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Http exposing (..)
import Json.Decode as D exposing (..)
import Server exposing (url)
import Shared
import Spa.Document exposing (Document)
import Spa.Page as Page exposing (Page)
import Spa.Url as Url exposing (Url)
import Json.Decode.Pipeline exposing (required)

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


type alias Post =
    { id : Int
    , name : String
    , ingredients : List String
    , recipe : String
    }

type alias Model =
    { profile : Data Profile
    , posts : Data (List Post)
    , warning : String
    }


type alias Profile =
    { id : Int
    , username : String
    , email : String
    , bio : String
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
    | ReceivedPosts (Data (List Post))


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
            { title = "Profile | " ++ profile.username
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
                ]
            }


viewProfile : Data Profile -> Html Msg
viewProfile profile =
    case profile of
        NotAsked ->
            text ""

        Loading ->
            div [ class "centered" ]
                [ img [ src "/assets/loading.gif" ] [] ]

        Success value ->
            div [ class "centered" ]
                [ h1 [ class "title_page" ] [ text "Profile" ]
                , div [ class "profile_attr" ]
                    [ p [ class "profile_name" ] [ text "username: " ]
                    , p [ class "profile_name_x" ] [ text value.username ]
                    ]
                , div [ class "profile_attr" ]
                    [ p [ class "profile_name" ] [ text "email: " ]
                    , p [ class "profile_name_x" ] [ text value.email ]
                    ]
                , div [ class "profile_attr" ]
                    [ p [ class "profile_name" ] [ text "bio: " ]
                    , p [ class "profile_name_x" ] [ text value.bio ]
                    ]
                ]

        Api.Data.Failure _ ->
            viewFetchError "profile" "Something went wrong!"


getContentRequest : Params -> { onResponse : Data (List Post) -> Msg } -> Cmd Msg
getContentRequest params options =
    Http.get
        { url = Server.url ++ "posts?userid=" ++ String.fromInt params.profileId ++ "&_sort=id&_order=desc"
        , expect = Api.Data.expectJson options.onResponse postsDecoder
        }

postsDecoder : Decoder (List Post)
postsDecoder =
    D.list postDecoder


postDecoder : Decoder Post
postDecoder =
    D.succeed Post
        |> Json.Decode.Pipeline.required "id" int
        |> Json.Decode.Pipeline.required "name" string
        |> Json.Decode.Pipeline.required "ingredients" (D.list string)
        |> Json.Decode.Pipeline.required "recipe" string


viewPosts : Data (List Post) -> Html Msg
viewPosts posts =
    case posts of
        NotAsked ->
            text ""

        Loading ->
            div [ class "centered" ]
                [ img [ src "/assets/loading.gif" ] [] ]

        Success actualPosts ->
            div [ class "centered" ]
                [ h1 [ class "title_page" ] [ text "My recipes" ]
                , div [ class "line_after_recipes" ] []
                , div [ class "articles_list" ]
                    (List.map viewPost actualPosts)
                ]

        Api.Data.Failure _ ->
            viewFetchError "posts" "Something went wrong!"



getUserRequest : Params -> { onResponse : Data Profile -> Msg } -> Cmd Msg
getUserRequest params options =
    Http.get
        { url = Server.url ++ "users/" ++ String.fromInt params.profileId
        , expect = Api.Data.expectJson options.onResponse userDecoder
        }


viewPost : Post -> Html Msg
viewPost post =
    ul [ class "post_list" ]
        [ li [ class "post_name" ]
            [ text post.name ]
        , p [ class "recipes_titles" ] [ text "ingredients" ]
        , li [ class "ingredients_names" ]
            [ String.join ", " post.ingredients |> text ]
        , p [ class "recipes_titles" ] [ text "recipe" ]
        , li [ class "recipe_names" ]
            [ text post.recipe ]
        , br [] []
        , a [ href ("/article/" ++ String.fromInt post.id) ] [ button [ class "submit_button" ] [ text "Comment" ] ]
        , div [ class "line_after_recipes" ] []
        ]


userDecoder : Decoder Profile
userDecoder =
    map4 Profile
        (field "id" D.int)
        (field "username" D.string)
        (field "email" D.string)
        (field "bio" D.string)


viewFetchError : String -> String -> Html Msg
viewFetchError items errorMessage =
    let
        errorHeading =
            "Couldn't fetch " ++ items ++ "."
    in
    div [ class "centered" ]
        [ h1 [ class "title_page" ] [ text errorHeading ]
        , text ("Error: " ++ errorMessage)
        ]
