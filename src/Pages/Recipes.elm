module Pages.Recipes exposing (Model, Msg, Params, page)

import Api.Article exposing (Article, articlesDecoder)
import Api.Data exposing (Data(..))
import Browser.Dom as Dom
import Browser.Navigation exposing (pushUrl)
import Components.TimeFormatting exposing (formatDate, formatTime)
import Dict
import Html exposing (..)
import Html.Attributes exposing (class, href, placeholder, src, type_, value, width)
import Html.Events exposing (onClick, onInput)
import Http exposing (..)
import Server exposing (url)
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


save : Model -> Shared.Model -> Shared.Model
save model shared =
    shared


load : Shared.Model -> Model -> ( Model, Cmd Msg )
load shared model =
    ( model, Cmd.none )



-- INIT


numberRecipesLimit =
    5


type alias Params =
    ()


type alias Model =
    { posts : Data (List Article)
    , paging : Int
    , search : String
    , sorting : String
    , order : String
    , totalCount : Int
    }


init : Shared.Model -> Url Params -> ( Model, Cmd Msg )
init shared { params } =
    ( initialModel
    , case shared.user of
        Just user_ ->
            Cmd.batch [ getContentRequest 1 "" "created" "desc" { onResponse = PostsReceived }, getContentRequestHeader 1 "" "created" "desc" ]

        Nothing ->
            pushUrl shared.key "/login"
    )


initialModel : Model
initialModel =
    { paging = 1, posts = Loading, search = "", sorting = "created", order = "desc", totalCount = 0 }



-- UPDATE


type Msg
    = PostsReceived (Data (List Article))
    | NoOp
    | Search String
    | ChangeSorting String String
    | ChangePaging Int
    | Tick Time.Posix
    | WatchCount (Result Http.Error Int)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        PostsReceived response ->
            ( { model | posts = response }, Cmd.none )

        Search searched ->
            ( { model | search = searched }, Cmd.batch [ getContentRequest model.paging searched model.sorting model.order { onResponse = PostsReceived }, getContentRequestHeader model.paging searched model.sorting model.order ] )

        ChangeSorting sorting order ->
            ( { model | sorting = sorting, order = order }, Cmd.batch [ getContentRequest model.paging model.search sorting order { onResponse = PostsReceived }, getContentRequestHeader model.paging model.search sorting order ] )

        Tick time ->
            ( model, Cmd.batch [ getContentRequest model.paging model.search model.sorting model.order { onResponse = PostsReceived }, getContentRequestHeader model.paging model.search model.sorting model.order ] )

        ChangePaging number ->
            ( { model | paging = number }, Cmd.batch [ getContentRequest number model.search model.sorting model.order { onResponse = PostsReceived }, getContentRequestHeader number model.search model.sorting model.order, resetViewport ] )

        WatchCount resp ->
            case resp of
                Ok value ->
                    ( { model | totalCount = value }, Cmd.none )

                Err _ ->
                    ( model, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    Time.every 30000 Tick



-- VIEW


view : Model -> Document Msg
view model =
    { title = "Recipes | GoodFood"
    , body =
        [ viewPosts model ]
    }


getContentRequest : Int -> String -> String -> String -> { onResponse : Data (List Article) -> Msg } -> Cmd Msg
getContentRequest paging searched sorting order options =
    Http.get
        { url = url ++ "/posts?_sort=" ++ sorting ++ "&_order=" ++ order ++ "&q=" ++ searched ++ "&_page=" ++ String.fromInt paging ++ "&_limit=" ++ String.fromInt numberRecipesLimit
        , expect = Api.Data.expectJson options.onResponse articlesDecoder
        }


getContentRequestHeader : Int -> String -> String -> String -> Cmd Msg
getContentRequestHeader paging searched sorting order =
    Http.get
        { url = url ++ "/posts?_sort=" ++ sorting ++ "&_order=" ++ order ++ "&q=" ++ searched ++ "&_page=" ++ String.fromInt paging ++ "&_limit=" ++ String.fromInt numberRecipesLimit
        , expect = expectHeader WatchCount
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
                    div []
                        [ div [ class "articles_list" ]
                            (List.map viewPost actualPosts)
                        , div []
                            (List.range 1
                                (if modBy numberRecipesLimit model.totalCount == 0 then
                                    model.totalCount // numberRecipesLimit

                                 else
                                    (model.totalCount // numberRecipesLimit) + 1
                                )
                                |> List.map (viewPages model)
                            )
                        ]
                ]

        Failure _ ->
            viewFetchError "Something went wrong!"


viewPages : Model -> Int -> Html Msg
viewPages model number =
    button
        [ if model.paging == number then
            class "page_numbers_active"

          else
            class "page_numbers"
        , onClick (ChangePaging number)
        ]
        [ text <| String.fromInt number ]


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
        , div [ class "justify__content" ]
            [ li [ class "value" ]
                [ renderList post.ingredients ]
            ]
        , p [ class "title" ] [ text "recipe" ]
        , div [ class "justify__content__recipe" ]
            [ li [ class "value" ]
                [ text post.recipe ]
            ]
        , p [ class "title" ] [ text "duration" ]
        , li [ class "value" ]
            [ text <| String.fromInt post.duration ++ " minutes" ]
        , p [ class "title" ] [ text "shared by" ]
        , li [ class "recipe_names" ]
            [ a [ class "link", href ("/profile/" ++ String.fromInt post.profile.id) ] [ text (post.profile.firstname ++ " " ++ post.profile.lastname) ]
            ]
        , img [ class "recipe__image", src post.image, width 500 ] []
        , br [] []
        , a [ href ("/article/" ++ String.fromInt post.id) ] [ button [ class "submit_button" ] [ text "Comment" ] ]
        , div [ class "line_after_recipes" ] []
        ]


renderList : List String -> Html msg
renderList lst =
    ol [ class "ingredients__" ]
        (List.map (\l -> li [ class "value" ] [ text l ]) lst)


expectHeader : (Result Http.Error Int -> msg) -> Expect msg
expectHeader toMsg =
    expectStringResponse toMsg <|
        \response ->
            case response of
                Http.BadUrl_ url ->
                    Err (Http.BadUrl url)

                Http.Timeout_ ->
                    Err Http.Timeout

                Http.NetworkError_ ->
                    Err Http.NetworkError

                Http.BadStatus_ metadata body ->
                    Err (Http.BadStatus metadata.statusCode)

                Http.GoodStatus_ metadata body ->
                    case Dict.get "x-total-count" metadata.headers of
                        Just number ->
                            Ok
                                (case String.toInt <| number of
                                    Nothing ->
                                        0

                                    Just number_ ->
                                        number_
                                )

                        Nothing ->
                            Ok 0


resetViewport : Cmd Msg
resetViewport =
    Task.perform (\_ -> NoOp) (Dom.setViewport 0 0)
