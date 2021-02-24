module Pages.Article exposing (Model, Msg, Params, page)

import Api.Data exposing (..)
import Browser.Navigation as Nav
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Http exposing (..)
import Json.Decode as D exposing (..)
import Json.Encode as E exposing (..)
import Server exposing (url)
import Shared
import Spa.Document exposing (Document)
import Spa.Page as Page exposing (Page)
import Spa.Url as Url exposing (Url)
import Api.Profile exposing (Profile, profileDecoder)
import Api.Article exposing (Article, articleDecoder)
import Api.Comment exposing (Comment, commentsDecoder, commentDecoder)


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
    , comment : Comment
    , warning : String
    }


init : Shared.Model -> Url Params -> ( Model, Cmd Msg )
init shared { params } =
    ( { article = Loading
      , comment = {comment = "", recipeid = params.recipeId, profile = {id = 1, firstname = "Patrik", lastname = "Villant", email = "Drogba11144@gmail.com", bio = "", image = ""}}
      , comments = Loading
      , warning = ""
      }
    , Cmd.batch [ getArticleRequest params { onResponse = ReceivedArticle }, getCommentsRequest params { onResponse = CommentsReceived } ]
    )



-- UPDATE


type Msg
    = ReceivedArticle (Data Article)
    | CommentsReceived (Data (List Comment))
    | AddComment String
    | SubmitComment
    | CommentResponse (Result Http.Error Comment)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ReceivedArticle response ->
            ( { model | article = response }, Cmd.none )

        CommentsReceived response ->
            ( { model | comments = response }, Cmd.none )

        AddComment comment ->
            ( { model
                | comment =
                    { comment = comment
                    , recipeid =
                        case model.article of
                            Success article ->
                                article.id

                            _ ->
                                0
                    , profile = {id = 1, firstname = "Patrik", lastname = "Villant", email = "Drogba11144@gmail.com", bio = "", image = ""}
                    }
              }
            , Cmd.none
            )

        SubmitComment ->
            if String.length model.comment.comment == 0 then
                ( { model | warning = "Type your comment!" }, Cmd.none )

            else
                ( model, Cmd.batch [ postComment model.comment, Nav.reloadAndSkipCache ] )

        CommentResponse response ->
            case response of
                Ok value ->
                    ( { model | warning = "Successfully added comment!" }, Cmd.none )

                Err err ->
                    ( { model | warning = "Something went wrong!" }, Cmd.none )


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
    case model.article of
        Success article ->
            { title = "Article | " ++ article.name 
            , body =
                [ viewArticle model.article
                , div [ class "warning_form" ] [ text model.warning ]
                , viewComments model.comments
                ]
            }

        _ ->
            { title = "Article"
            , body = []
            }


getArticleRequest : Params -> { onResponse : Data Article -> Msg } -> Cmd Msg
getArticleRequest params options =
    Http.get
        { url = Server.url ++ "posts/" ++ String.fromInt params.recipeId
        , expect = Api.Data.expectJson options.onResponse articleDecoder
        }


getCommentsRequest : Params -> { onResponse : Data (List Comment) -> Msg } -> Cmd Msg
getCommentsRequest params options =
    Http.get
        { url = Server.url ++ "comments?recipeid=" ++ String.fromInt params.recipeId
        , expect = Api.Data.expectJson options.onResponse commentsDecoder
        }

viewComments : Data (List Comment) -> Html Msg
viewComments comments =
    case comments of
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

        Api.Data.Failure _ ->
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
        , a [ class "comment_content", href ("/profile/" ++ String.fromInt comment.profile.id) ] [ text (comment.profile.firstname ++ " " ++ comment.profile.lastname)]
        , div [ class "line_after_recipes" ] []
        ]


encodeComment : Comment -> E.Value
encodeComment comment =
    E.object
        [ ( "comment", E.string comment.comment )
        , ( "recipeid", E.int comment.recipeid )
        , ( "profile"
          , E.object
                [ ("id", E.int 1)
                , ( "email", E.string "Drogba11144@gmail.com" )
                , ( "firstname", E.string "Patrik" )
                , ( "lastname", E.string "Villant" )
                , ( "bio", E.string "" )
                , ( "password", E.string "" )
                , ( "image", E.string "" )
                ]
          )
        ]


postComment : Comment -> Cmd Msg
postComment comment =
    Http.post
        { url = Server.url ++ "comments/"
        , body = Http.jsonBody <| encodeComment comment
        , expect = Http.expectJson CommentResponse commentDecoder
        }


viewArticle : Data Article -> Html Msg
viewArticle article =
    case article of
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
                    [ p [ class "profile_name" ] [ text "shared by: " ]
                    , a [ class "link_profile", href ("/profile/" ++ String.fromInt value.profile.id) ] [ text (value.profile.firstname ++ " " ++ value.profile.lastname) ]
                    ]
                , div [ class "comment-text" ]
                    [ textarea [ placeholder "Type your comment here...", cols 70, rows 10, onInput AddComment ] []
                    ]
                , div [ class "comment-button" ]
                    [ button [ class "submit_button", onClick SubmitComment ] [ text "Share comment" ]
                    ]
                ]

        Api.Data.Failure _ ->
            viewFetchError "article" "Something went wrong!"
