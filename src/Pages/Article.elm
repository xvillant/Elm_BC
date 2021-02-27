module Pages.Article exposing (Model, Msg, Params, page)

import Api.Article exposing (Article, articleDecoder)
import Api.Comment exposing (Comment, commentDecoder, commentsDecoder)
import Api.Data exposing (Data(..))
import Api.Profile exposing (Profile, profileDecoder)
import Html exposing (..)
import Html.Attributes exposing (class, cols, href, placeholder, rows, src)
import Html.Events as Events exposing (onClick, onInput)
import Http exposing (..)
import Iso8601
import Json.Encode as E exposing (..)
import Server exposing (url)
import Shared
import Spa.Document exposing (Document)
import Spa.Page as Page exposing (Page)
import Spa.Url as Url exposing (Url)
import Task
import Time
import TimeFormatting exposing (formatDate, formatTime)


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
    { recipeId : Int }


type alias Model =
    { article : Data Article
    , comments : Data (List Comment)
    , commentString : String
    , warning : String
    , time : Time.Posix
    }


init : Shared.Model -> Url Params -> ( Model, Cmd Msg )
init shared { params } =
    ( { article = Loading
      , commentString = ""
      , comments = Loading
      , warning = ""
      , time = Time.millisToPosix 0
      }
    , Cmd.batch [ getArticleRequest params { onResponse = ReceivedArticle }, getCommentsRequest params { onResponse = CommentsReceived }, Task.perform GetTime Time.now ]
    )



-- UPDATE


type Msg
    = ReceivedArticle (Data Article)
    | CommentsReceived (Data (List Comment))
    | AddComment String
    | SubmitComment
    | GetTime Time.Posix
    | CommentResponse (Data Comment)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ReceivedArticle response ->
            ( { model | article = response }, Cmd.none )

        CommentsReceived response ->
            ( { model | comments = response }, Cmd.none )

        AddComment comment ->
            ( { model | commentString = comment }, Cmd.none )

        SubmitComment ->
            if String.isEmpty model.commentString then
                ( { model | warning = "Type your comment!" }, Cmd.none )

            else
                ( { model | commentString = "" }, postComment model { onResponse = CommentResponse }
                )

        GetTime time ->
            ( { model | time = time }, Cmd.none )

        CommentResponse comment ->
            ( case comment of
                Success c ->
                    { model | comments = Api.Data.map (\comments -> c :: comments) model.comments, commentString = "" }

                _ ->
                    model
            , Cmd.none
            )


save : Model -> Shared.Model -> Shared.Model
save model shared =
    shared


load : Shared.Model -> Model -> ( Model, Cmd Msg )
load shared model =
    ( model, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    Time.every 1000 GetTime



-- VIEW


view : Model -> Document Msg
view model =
    case model.article of
        Success article ->
            { title = "Article | " ++ article.name
            , body =
                [ viewArticle model
                , div [ class "warning_form" ] [ text model.warning ]
                , viewComments model
                ]
            }

        _ ->
            { title = "Article"
            , body = [
                viewArticle model
                , div [ class "warning_form" ] [ text model.warning ]
                , viewComments model
            ]
            }


getArticleRequest : Params -> { onResponse : Data Article -> Msg } -> Cmd Msg
getArticleRequest params options =
    Http.get
        { url = Server.url ++ "/posts/" ++ String.fromInt params.recipeId
        , expect = Api.Data.expectJson options.onResponse articleDecoder
        }


getCommentsRequest : Params -> { onResponse : Data (List Comment) -> Msg } -> Cmd Msg
getCommentsRequest params options =
    Http.get
        { url = Server.url ++ "/comments?recipeid=" ++ String.fromInt params.recipeId
        , expect = Api.Data.expectJson options.onResponse commentsDecoder
        }


viewComments : Model -> Html Msg
viewComments model =
    case model.comments of
        NotAsked ->
            text ""

        Loading ->
            div [ class "centered" ]
                [ img [ src "/assets/loading.gif" ] [] ]

        Success actualComments ->
            div [ class "centered" ]
                [ h1 [ class "title_comment" ] [ text "Comments" ]
                , div [ class "line_after_recipes" ] []
                , div [ class "comments_list" ]
                    (List.map viewComment actualComments)
                ]

        Failure _ ->
            viewFetchError "comments" "Something went wrong!"


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


viewComment : Comment -> Html Msg
viewComment comment =
    ul [ class "comment_list" ]
        [ li [ class "comment_content" ]
            [ text comment.comment ]
        , li [ class "comment_content" ]
            [ text (comment.created |> formatDate) ]
        , li [ class "comment_content" ]
            [ text (comment.created |> formatTime) ]
        , a [ class "comment_content", href ("/profile/" ++ String.fromInt comment.profile.id) ] [ text (comment.profile.firstname ++ " " ++ comment.profile.lastname) ]
        , div [ class "line_after_recipes" ] []
        ]


encodeComment : Model -> E.Value
encodeComment model =
    E.object
        [ ( "comment", E.string model.commentString )
        , ( "recipeid"
          , E.int
                (case model.article of
                    Success article ->
                        article.id

                    _ ->
                        0
                )
          )
        , ( "profile"
          , E.object
                [ ( "id", E.int 1 )
                , ( "email", E.string "Drogba11144@gmail.com" )
                , ( "firstname", E.string "Patrik" )
                , ( "lastname", E.string "Villant" )
                , ( "bio", E.string "" )
                , ( "password", E.string "" )
                , ( "image", E.string "" )
                , ( "created", E.string "1970-01-01T00:00:00.000Z" )
                ]
          )
        , ( "created", Iso8601.encode model.time )
        ]


postComment : Model -> { onResponse : Data Comment -> Msg } -> Cmd Msg
postComment model options =
    Http.post
        { url = Server.url ++ "/comments"
        , body = Http.jsonBody <| encodeComment model
        , expect = Api.Data.expectJson options.onResponse commentDecoder
        }


viewArticle : Model -> Html Msg
viewArticle model =
    case model.article of
        NotAsked ->
            text ""

        Loading ->
            div [ class "centered" ]
                [ img [ src "/assets/loading.gif" ] [] ]

        Success value ->
            div [ class "centered" ]
                [ h1 [ class "title_page" ] [ text "Article" ]
                , div [ class "profile_attr" ]
                    [ p [ class "profile_name" ] [ text "recipe name: " ]
                    , p [ class "profile_name_x" ] [ text value.name ]
                    ]
                , div [ class "profile_attr" ]
                    [ p [ class "profile_name" ] [ text "ingredients: " ]
                    , p [ class "profile_name_x" ] [ text <| String.join ", " value.ingredients ]
                    ]
                , div [ class "profile_attr" ]
                    [ p [ class "profile_name" ] [ text "recipe: " ]
                    , p [ class "profile_name_x" ] [ text value.recipe ]
                    ]
                , div [ class "profile_attr" ]
                    [ p [ class "profile_name" ] [ text "shared at: " ]
                    , p [ class "profile_name_x" ] [ text (value.created |> formatDate) ]
                    , p [ class "profile_name_x" ] [ text (value.created |> formatTime) ]
                    ]
                , div [ class "profile_attr" ]
                    [ p [ class "profile_name" ] [ text "shared by: " ]
                    , a [ class "link_profile", href ("/profile/" ++ String.fromInt value.profile.id) ] [ text (value.profile.firstname ++ " " ++ value.profile.lastname) ]
                    ]
                , div [ class "comment-text" ]
                    [ textarea [ placeholder "Type your comment here...", cols 70, rows 10, Html.Attributes.value model.commentString, onInput AddComment ] []
                    ]
                , div [ class "comment-button" ]
                    [ button [ class "submit_button", onClick SubmitComment ] [ text "Share comment" ]
                    ]
                ]

        Failure _ ->
            viewFetchError "article" "Something went wrong!"
