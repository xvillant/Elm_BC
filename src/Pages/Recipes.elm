module Pages.Recipes exposing (Model, Msg, Params, page)

import Api.Data exposing (..)
import Html exposing (..)
import Html.Attributes exposing (class, href, placeholder, type_, src)
import Html.Events exposing (onClick, onInput)
import Http exposing (..)
import Server
import Shared
import Spa.Document exposing (Document)
import Spa.Page as Page exposing (Page)
import Spa.Url as Url exposing (Url)
import Api.Article exposing (articlesDecoder, Article)

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
    ( initialModel, getContentRequest initialModel { onResponse = PostsReceived } )


initialModel : Model
initialModel =
    { posts = Loading, search = "", sorting = "id", order = "desc"}



-- UPDATE


type Msg =
    PostsReceived (Data (List Article))
    | Search String
    | ChangeSorting String String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        PostsReceived response ->
            ( { model | posts = response }, Cmd.none )

        Search searched ->
            ( { model | search = searched }, getContentRequest model { onResponse = PostsReceived } )

        ChangeSorting sorting order ->
            ( { model | sorting = sorting, order = order }, getContentRequest model { onResponse = PostsReceived } )

subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Model -> Document Msg
view model =
    { title = "Recipes"
    , body =
        [ viewPosts model model.posts
        ]
    }


getContentRequest : Model -> { onResponse : Data (List Article) -> Msg } -> Cmd Msg
getContentRequest model options =
    Http.get
        { url = Server.url ++ "posts?_sort=" ++ model.sorting ++ "&_order="++ model.order ++"&q=" ++ model.search
        , expect = Api.Data.expectJson options.onResponse articlesDecoder
        }



viewPosts : Model -> Data (List Article) -> Html Msg
viewPosts model posts =
    case posts of
        NotAsked ->
            text ""

        Loading ->
            div [ class "centered" ]
                [ img [ src "/assets/loading.gif" ] [] ]

        Success actualPosts ->
            div [ class "centered" ]
                [ h1 [ class "title_page" ] [ text "Recipes" ]
                , button [ case model.sorting of 
                    "id" -> class "active_buttonsdiv"
                    _ -> class "buttonsdiv"
                , onClick <| ChangeSorting "id" "desc" ] [ text "Latest posts" ]
                , button [ case model.sorting of 
                    "name" -> class "active_buttonsdiv"
                    _ -> class "buttonsdiv", onClick <| ChangeSorting "name" "asc" ] [ text "Sort by name" ]
                , input [ class "search_input", type_ "search", placeholder "Search...", onInput Search ] []
                , div [ class "line_after_recipes" ] []
                , div [ class "articles_list" ]
                    (List.map viewPost actualPosts)
                ]

        Api.Data.Failure _ ->
            viewFetchError "Something went wrong!"


viewFetchError : String -> Html Msg
viewFetchError errorMessage =
    let
        errorHeading =
            "Couldn't fetch recipes."
    in
    div [ class "centered" ]
        [ h1 [ class "title_page" ] [ text errorHeading ]
        , text ("Error: " ++ errorMessage)
        ]


viewPost : Article -> Html Msg
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
        , p [ class "recipes_titles" ] [ text "shared by" ]
        , li [ class "recipe_names" ][
            a [ class "link_profile", href ("/profile/" ++ String.fromInt post.profile.id) ][ text (post.profile.firstname ++ " " ++post.profile.lastname)]]
        , br [] []
        , a [ href ("/article/" ++ String.fromInt post.id) ] [ button [ class "submit_button" ] [ text "Comment" ] ]
        , div [ class "line_after_recipes" ] []
        ]
