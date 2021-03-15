module Pages.New exposing (Model, Msg, Params, page)

import Api.Article exposing (Article, articleDecoder)
import Api.Data exposing (Data(..))
import Api.User exposing (User)
import Array exposing (push)
import Browser.Navigation exposing (Key, pushUrl)
import Elm.Module exposing (Name)
import FeatherIcons exposing (user)
import Html exposing (..)
import Html.Attributes exposing (autocomplete, class, cols, id, placeholder, rows, type_, value)
import Html.Events exposing (onClick, onInput)
import Http exposing (..)
import Iso8601
import Json.Encode as E exposing (..)
import Server
import Shared exposing (Model)
import Spa.Document exposing (Document)
import Spa.Page as Page exposing (Page)
import Spa.Url exposing (Url)
import String.Extra
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
    ()


type alias Model =
    { name : String
    , ingredients : String
    , ingredientsList : List String
    , recipe : String
    , warning : String
    , duration : String
    , key : Key
    , user : Maybe User
    }


init : Shared.Model -> Url Params -> ( Model, Cmd Msg )
init shared { params } =
    ( initialModel shared
    , case shared.user of
        Just user_ ->
            Cmd.none

        Nothing ->
            pushUrl shared.key "/login"
    )


initialModel : Shared.Model -> Model
initialModel shared =
    { name = "", ingredients = "", recipe = "", warning = "", key = shared.key, user = shared.user, duration = "", ingredientsList = [] }



-- UPDATE


type Msg
    = Name String
    | Ingredients String
    | Recipe String
    | Duration String
    | Submit Time.Posix
    | GetTime (Time.Posix -> Msg)
    | Response (Data Article)
    | AddToIngredients


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Name name ->
            ( { model | name = name, warning = "" }, Cmd.none )

        Ingredients ingredients ->
            ( { model | ingredients = ingredients, warning = "" }, Cmd.none )

        Recipe recipe ->
            ( { model | recipe = recipe, warning = "" }, Cmd.none )

        Duration duration ->
            ( { model | duration = duration, warning = "" }, Cmd.none )

        GetTime time ->
            ( model, Task.perform time Time.now )

        Submit time ->
            if String.isEmpty model.name then
                ( { model | warning = "Enter recipe name!" }, Cmd.none )

            else if List.isEmpty model.ingredientsList then
                ( { model | warning = "Enter ingredients!" }, Cmd.none )

            else if String.isEmpty model.recipe then
                ( { model | warning = "Enter recipe!" }, Cmd.none )

            else if
                String.isEmpty model.duration
                    || (case String.toInt model.duration of
                            Nothing ->
                                1

                            Just number ->
                                number
                       )
                    <= 0
            then
                ( { model | warning = "Enter a valid duration in minutes!" }, Cmd.none )

            else
                ( { model | warning = "Loading..." }, postArticle time model { onResponse = Response } )

        AddToIngredients ->
            ( { model
                | ingredients = ""
                , ingredientsList =
                    if String.isEmpty model.ingredients then
                        model.ingredientsList

                    else
                        model.ingredientsList ++ [ String.trim <| String.toLower model.ingredients ]
                , warning =
                    if String.isEmpty model.ingredients then
                        "Enter ingredient!"

                    else
                        ""
              }
            , Cmd.none
            )

        Response response ->
            case response of
                Success s ->
                    ( { model | warning = "Successfully added article!" }, pushUrl model.key "/recipes" )

                _ ->
                    ( { model | warning = "Something went wrong!" }, Cmd.none )


save : Model -> Shared.Model -> Shared.Model
save model shared =
    shared


load : Shared.Model -> Model -> ( Model, Cmd Msg )
load shared model =
    ( { model | user = shared.user }, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Model -> Document Msg
view model =
    { title = "New Article | GoodFood"
    , body =
        [ div []
            [ h1 [] [ text "New Article" ]
            , div []
                [ input
                    [ id "recipe_name"
                    , type_ "text"
                    , placeholder "Recipe Name"
                    , autocomplete False
                    , value model.name
                    , onInput Name
                    , class "form"
                    ]
                    []
                ]
            , div []
                [ input
                    [ id "ingredients"
                    , type_ "text"
                    , autocomplete False
                    , placeholder "Type your ingredient"
                    , value model.ingredients
                    , onInput Ingredients
                    , class "form"
                    ]
                    []
                ]
            , button [ class "submit_button", onClick AddToIngredients ] [ text "Add ingredient" ]
            , div [ class "ingerdients__container" ]
                [ if List.isEmpty model.ingredientsList then
                    text "No ingredients added yet..."

                  else
                    renderList model.ingredientsList
                ]
            , div []
                [ textarea
                    [ id "recipe"
                    , value model.recipe
                    , placeholder "Type here the recipe"
                    , onInput Recipe
                    , rows 10
                    , cols 70
                    , class "form"
                    ]
                    []
                ]
            , div []
                [ input
                    [ id "duration"
                    , type_ "number"
                    , autocomplete False
                    , Html.Attributes.min "1"
                    , placeholder "Type duration in minutes"
                    , value model.duration
                    , onInput Duration
                    , class "form"
                    ]
                    []
                ]
            , div []
                [ button [ class "submit_button", onClick <| GetTime Submit ] [ text "Share recipe" ] ]
            , div [ class "warning_form" ]
                [ text model.warning ]
            ]
        ]
    }


postArticle : Time.Posix -> Model -> { onResponse : Data Article -> Msg } -> Cmd Msg
postArticle nowTime model options =
    let
        body =
            [ ( "name", E.string <| String.Extra.toSentenceCase <| String.toLower <| model.name )
            , ( "ingredients", E.list E.string model.ingredientsList )
            , ( "recipe", E.string model.recipe )
            , ( "duration"
              , E.int
                    (case String.toInt model.duration of
                        Nothing ->
                            0

                        Just number ->
                            number
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
            , ( "duration"
              , E.int
                    (case String.toInt model.duration of
                        Nothing ->
                            0

                        Just number ->
                            number
                    )
              )
            ]
                |> E.object
                |> Http.jsonBody
    in
    Http.post
        { url = Server.url ++ "/posts"
        , body = body
        , expect = Api.Data.expectJson options.onResponse articleDecoder
        }


renderList : List String -> Html msg
renderList lst =
    ol [ class "ingredients__" ]
        (List.map (\l -> li [ class "list__ingredients" ] [ text l ]) lst)
