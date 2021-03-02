module Pages.Article.RecipeId_Int exposing (Model, Msg, Params, page)

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
    { recipeId : Int }


type alias Model =
    { article : Data Article
    , comments : Data (List Comment)
    , commentString : String
    , warning : String
    , time : Time.Posix
    , zone : Time.Zone
    }


init : Shared.Model -> Url Params -> ( Model, Cmd Msg )
init shared { params } =
    ( { article = Loading
      , commentString = ""
      , comments = Loading
      , warning = ""
      , time = Time.millisToPosix 0
      , zone = Time.utc
      }
    , Cmd.batch [ getArticleRequest params { onResponse = ReceivedArticle }, getCommentsRequest params { onResponse = CommentsReceived }, Task.perform GetTime Time.now, Task.perform Timezone Time.here ]
    )



-- UPDATE


type Msg
    = ReceivedArticle (Data Article)
    | CommentsReceived (Data (List Comment))
    | AddComment String
    | SubmitComment
    | GetTime Time.Posix
    | Timezone Time.Zone
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
                ( { model | commentString = "" }
                , postComment model { onResponse = CommentResponse }
                )

        Timezone tz ->
            ( { model | zone = tz }, Cmd.none )

        GetTime time ->
            ( { model | time = time }, Cmd.none )

        CommentResponse comment ->
            ( case comment of
                Success c ->
                    { model | comments = Api.Data.map (\comments -> c :: comments) model.comments, commentString = "", warning = "" }

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
            , body =
                [ viewArticle model
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
        { url = Server.url ++ "/comments?recipeid=" ++ String.fromInt params.recipeId ++ "&_sort=created&_order=desc"
        , expect = Api.Data.expectJson options.onResponse commentsDecoder
        }


viewComments : Model -> Html Msg
viewComments model =
    case model.comments of
        Success actualComments ->
            if List.isEmpty actualComments then
                div []
                    [ h2 [] [ text "Comments" ]
                    , div [ class "line_after_recipes" ] []
                    , div [ class "comments_list" ]
                        [ br [] []
                        , p [ class "err" ] [ text "No comments yet..." ]
                        ]
                    ]

            else
                div []
                    [ h2 [] [ text "Comments" ]
                    , div [ class "line_after_recipes" ] []
                    , div [ class "comments_list" ]
                        (List.map viewComment actualComments)
                    ]

        _ ->
            text ""


viewFetchError : String -> String -> Html Msg
viewFetchError items errorMessage =
    let
        errorHeading =
            "Couldn't fetch " ++ items ++ "."
    in
    div []
        [ h1 [] [ text errorHeading ]
        , text ("Error: " ++ errorMessage)
        ]


viewComment : Comment -> Html Msg
viewComment comment =
    let
        timezone =
            europe__bratislava ()
    in
    ul []
        [ li [ class "value" ]
            [ text comment.comment ]
        , li [ class "value" ]
            [ p [ class "datetime" ] [ text (formatDate timezone comment.created) ]
            , p [ class "datetime" ] [ text (formatTime timezone comment.created) ]
            ]
        , a [ class "link", href ("/profile/" ++ String.fromInt comment.profile.id) ] [ text (comment.profile.firstname ++ " " ++ comment.profile.lastname) ]
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
            div []
                [ img [ src "/assets/loading.gif" ] [] ]

        Success value ->
            div []
                [ h1 [] [ text value.name ]
                , p [ class "datetime" ] [ text (formatDate model.zone value.created) ]
                , p [ class "datetime" ] [ text (formatTime model.zone value.created) ]
                , div []
                    [ p [ class "title" ] [ text "ingredients " ]
                    , p [ class "value" ] [ text <| String.join ", " value.ingredients ]
                    ]
                , div []
                    [ p [ class "title" ] [ text "recipe " ]
                    , p [ class "value" ] [ text value.recipe ]
                    ]
                , div []
                    [ p [ class "title" ] [ text "shared by " ]
                    , a [ class "link", href ("/profile/" ++ String.fromInt value.profile.id) ] [ text (value.profile.firstname ++ " " ++ value.profile.lastname) ]
                    ]
                , div []
                    [ textarea [ placeholder "Type your comment here...", cols 70, rows 10, Html.Attributes.value model.commentString, onInput AddComment, class "form" ] []
                    ]
                , div []
                    [ button [ class "submit_button", onClick SubmitComment ] [ text "Share comment" ]
                    ]
                ]

        Failure _ ->
            viewFetchError "article" "Something went wrong!"
