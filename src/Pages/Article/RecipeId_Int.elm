module Pages.Article.RecipeId_Int exposing (Model, Msg, Params, page)

--import Api.Profile exposing (Profile, profileDecoder)

import Api.Article exposing (Article, articleDecoder)
import Api.Comment exposing (Comment, commentDecoder, commentsDecoder)
import Api.Data exposing (Data(..))
import Api.User exposing (User)
import Browser.Navigation exposing (pushUrl)
import Components.TimeFormatting exposing (formatDate, formatTime)
import Html exposing (..)
import Html.Attributes exposing (class, cols, href, placeholder, rows, src)
import Html.Events exposing (onClick, onInput)
import Http exposing (..)
import Iso8601
import Json.Encode as E exposing (..)
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
    { recipeId : Int }


type alias Model =
    { article : Data Article
    , comments : Data (List Comment)
    , commentString : String
    , warning : String
    , zone : Time.Zone
    , user : Maybe User
    , parameters : Params
    }


init : Shared.Model -> Url Params -> ( Model, Cmd Msg )
init shared { params } =
    ( { article = Loading
      , commentString = ""
      , comments = Loading
      , warning = ""
      , zone = Time.utc
      , user = shared.user
      , parameters = params
      }
    , case shared.user of
        Just user_ ->
            Cmd.batch [ getArticleRequest params { onResponse = ReceivedArticle }, getCommentsRequest params { onResponse = CommentsReceived }, Task.perform Timezone Time.here ]

        Nothing ->
            pushUrl shared.key "/login"
    )



-- UPDATE


type Msg
    = ReceivedArticle (Data Article)
    | CommentsReceived (Data (List Comment))
    | AddComment String
    | SubmitComment Time.Posix
    | GetTime (Time.Posix -> Msg)
    | Timezone Time.Zone
    | CommentResponse (Data Comment)
    | Tick Time.Posix


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ReceivedArticle response ->
            ( { model | article = response }, Cmd.none )

        CommentsReceived response ->
            ( { model | comments = response }, Cmd.none )

        AddComment comment ->
            ( { model | commentString = comment, warning = ""}, Cmd.none )

        SubmitComment time ->
            if String.isEmpty model.commentString then
                ( { model | warning = "Type your comment!" }, Cmd.none )

            else
                ( { model | commentString = "" }
                , postComment time model { onResponse = CommentResponse }
                )

        Timezone tz ->
            ( { model | zone = tz }, Cmd.none )

        GetTime time ->
            ( model, Task.perform time Time.now )

        CommentResponse comment ->
            ( case comment of
                Success c ->
                    { model | comments = Api.Data.map (\comments -> c :: comments) model.comments, commentString = "", warning = "" }

                _ ->
                    model
            , Cmd.none
            )

        Tick time ->
            ( model, getCommentsRequest model.parameters { onResponse = CommentsReceived } )


save : Model -> Shared.Model -> Shared.Model
save model shared =
    shared


load : Shared.Model -> Model -> ( Model, Cmd Msg )
load shared model =
    ( { model | user = shared.user }, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    Time.every 30000 Tick



-- VIEW


view : Model -> Document Msg
view model =
    case model.article of
        Success article ->
            { title = "Article | " ++ article.name ++ " | GoodFood"
            , body =
                [ viewArticle model
                , div [ class "warning_form" ] [ text model.warning ]
                , viewComments model
                ]
            }

        _ ->
            { title = "Article | GoodFood"
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


postComment : Time.Posix -> Model -> { onResponse : Data Comment -> Msg } -> Cmd Msg
postComment nowTime model options =
    let
        body =
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
              , case model.user of
                    Just user ->
                        E.object
                            [ ( "id", E.int user.id )
                            , ( "email", E.string user.email )
                            , ( "firstname", E.string user.firstname )
                            , ( "lastname", E.string user.lastname )
                            , ( "bio", E.string user.bio )
                            , ( "password", E.string user.password )
                            , ( "image", E.string user.image )
                            , ( "created", Iso8601.encode user.created )
                            ]

                    Nothing ->
                        E.object
                            [ ( "id", E.int 0 )
                            , ( "email", E.string "" )
                            , ( "firstname", E.string "" )
                            , ( "lastname", E.string "" )
                            , ( "bio", E.string "" )
                            , ( "password", E.string "" )
                            , ( "image", E.string "" )
                            , ( "created", E.string "" )
                            ]
              )
            , ( "created", Iso8601.encode nowTime )
            ]
                |> E.object
                |> Http.jsonBody
    in
    Http.post
        { url = Server.url ++ "/comments"
        , body = body
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
                    [ button [ class "submit_button", onClick <| GetTime SubmitComment ] [ text "Share comment" ]
                    ]
                ]

        Failure _ ->
            viewFetchError "article" "Something went wrong!"
