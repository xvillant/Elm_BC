module Pages.Recipes exposing (Model, Msg, Params, page)

import Api.Article exposing (Article, articlesDecoder)
import Api.Data exposing (Data(..))
import Html exposing (..)
import Html.Attributes exposing (class, href, placeholder, src, type_, value)
import Html.Events exposing (onClick, onInput)
import Http exposing (..)
import Server exposing (url)
import Shared
import Spa.Document exposing (Document)
import Spa.Page as Page exposing (Page)
import Spa.Url exposing (Url)
import Time
import Components.TimeFormatting exposing (formatDate, formatTime)
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


save : Model -> Shared.Model -> Shared.Model
save model shared =
    shared


load : Shared.Model -> Model -> ( Model, Cmd Msg )
load shared model =
    ( model, Cmd.none )



-- INIT


type alias Params =
    ()


type alias Model =
    { posts : Data (List Article)
    , search : String
    , sorting : String
    , order : String
    }


init : Shared.Model -> Url Params -> ( Model, Cmd Msg )
init shared { params } =
    ( initialModel, getContentRequest "" "created" "desc" { onResponse = PostsReceived } )


initialModel : Model
initialModel =
    { posts = Loading, search = "", sorting = "created", order = "desc" }



-- UPDATE


type Msg
    = PostsReceived (Data (List Article))
    | Search String
    | ChangeSorting String String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        PostsReceived response ->
            ( { model | posts = response }, Cmd.none )

        Search searched ->
            ( { model | search = searched }, getContentRequest searched model.sorting model.order { onResponse = PostsReceived } )

        ChangeSorting sorting order ->
            ( { model | sorting = sorting, order = order }, getContentRequest model.search sorting order { onResponse = PostsReceived } )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Model -> Document Msg
view model =
    { title = "Recipes | GoodFood"
    , body =
        [ viewPosts model ]
    }


getContentRequest : String -> String -> String -> { onResponse : Data (List Article) -> Msg } -> Cmd Msg
getContentRequest searched sorting order options =
    Http.get
        { url = url ++ "/posts?_sort=" ++ sorting ++ "&_order=" ++ order ++ "&q=" ++ searched
        , expect = Api.Data.expectJson options.onResponse articlesDecoder
        }


viewPosts : Model -> Html Msg
viewPosts model =
    case model.posts of
        NotAsked ->
            text ""

        Loading ->
            div []
                [ img [ src "/assets/loading.gif" ] [] ]

        Success actualPosts ->
            div []
                [ h1 [] [ text "Recipes" ]
                , button
                    [ case model.sorting of
                        "created" ->
                            class "active_category_button"

                        _ ->
                            class "category_button"
                    , onClick <| ChangeSorting "created" "desc"
                    ]
                    [ text "Latest posts" ]
                , button
                    [ case model.sorting of
                        "name" ->
                            class "active_category_button"

                        _ ->
                            class "category_button"
                    , onClick <| ChangeSorting "name" "asc"
                    ]
                    [ text "Sort by name" ]
                , br [] []
                , input [ class "search_input", type_ "search", placeholder "Search...", onInput Search, value model.search ] []
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


viewPost : Article -> Html Msg
viewPost post =
    let
        timezone =
            europe__bratislava ()
    in
    ul [ class "post_list" ]
        [ h2 [ class "post_name" ]
            [ text post.name ]
        , p [ class "datetime" ] [ text (formatDate timezone post.created) ]
        , p [ class "datetime" ] [ text (formatTime timezone post.created) ]
        , p [ class "title" ] [ text "ingredients" ]
        , li [ class "value" ]
            [ String.join ", " post.ingredients |> text ]
        , p [ class "title" ] [ text "recipe" ]
        , li [ class "value" ]
            [ text post.recipe ]
        , p [ class "title" ] [ text "shared by" ]
        , li [ class "recipe_names" ]
            [ a [ class "link", href ("/profile/" ++ String.fromInt post.profile.id) ] [ text (post.profile.firstname ++ " " ++ post.profile.lastname) ]
            ]
        , br [] []
        , a [ href ("/article/" ++ String.fromInt post.id) ] [ button [ class "submit_button" ] [ text "Comment" ] ]
        , div [ class "line_after_recipes" ] []
        ]

