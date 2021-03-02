module Pages.New exposing (Model, Msg, Params, page)

import Api.Article exposing (Article, articleDecoder)
import Api.Data exposing (Data(..))
import Browser.Navigation as Nav exposing (Key, pushUrl)
import Elm.Module exposing (Name)
import Html exposing (..)
import Html.Attributes exposing (autocomplete, class, cols, id, placeholder, rows, type_, value)
import Html.Events exposing (onClick, onInput)
import Http exposing (..)
import Iso8601
import Json.Decode as D exposing (field)
import Json.Encode as E exposing (..)
import Server exposing (url)
import Shared exposing (Model)
import Spa.Document exposing (Document)
import Spa.Page as Page exposing (Page)
import Spa.Url as Url exposing (Url)
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
    , recipe : String
    , warning : String
    , time : Time.Posix
    , key : Key
    }


init : Shared.Model -> Url Params -> ( Model, Cmd Msg )
init shared { params } =
    ( { name = "", ingredients = "", recipe = "", warning = "", key = shared.key, time = Time.millisToPosix 0 }, Task.perform GetTime Time.now )



-- UPDATE


type Msg
    = Name String
    | Ingredients String
    | Recipe String
    | Submit
    | GetTime Time.Posix
    | Response (Data Article)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Name name ->
            ( { model | name = name }, Cmd.none )

        Ingredients ingredients ->
            ( { model | ingredients = ingredients }, Cmd.none )

        Recipe recipe ->
            ( { model | recipe = recipe }, Cmd.none )

        Submit ->
            if String.length model.name == 0 then
                ( { model | warning = "Enter recipe name!" }, Cmd.none )

            else if String.length model.ingredients == 0 then
                ( { model | warning = "Enter ingredients!" }, Cmd.none )

            else if String.length model.recipe == 0 then
                ( { model | warning = "Enter recipe!" }, Cmd.none )

            else
                ( { model | warning = "Loading..." }, postArticle model { onResponse = Response } ) 

        GetTime time ->
            ( { model | time = time }, Cmd.none )

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
    ( model, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    Time.every 1000 GetTime



-- VIEW


view : Model -> Document Msg
view model =
    { title = "New Article"
    , body =
        [ div [ ]
            [ h1 [ ] [ text "New Article" ]
            , div [ ]
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
            , div [ ]
                [ input
                    [ id "ingredients"
                    , type_ "text"
                    , autocomplete False
                    , placeholder "Ingredients - divide (,)"
                    , value model.ingredients
                    , onInput Ingredients
                    , class "form"
                    ]
                    []
                ]
            , div [ ]
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
            , div [ ]
                [ button [ class "submit_button", onClick Submit ] [ text "Share recipe" ] ]
            , div [ class "warning_form" ]
                [ text model.warning ]
            ]
        ]
    }


postArticle : Model -> { onResponse : Data Article -> Msg } -> Cmd Msg
postArticle model options =
    let
        body =
            [ ( "name", E.string <| String.Extra.toSentenceCase <| model.name )
            , ( "ingredients", E.list E.string <| List.map String.trim <| String.split "," model.ingredients )
            , ( "recipe", E.string model.recipe )
            , ( "profile"
              , E.object
                    [ ( "id", E.int 1 )
                    , ( "email", E.string "Drogba11144@gmail.com" )
                    , ( "firstname", E.string "Patrik" )
                    , ( "lastname", E.string "Villant" )
                    , ( "bio", E.string "" )
                    , ( "password", E.string "" )
                    , ( "image", E.string "" )
                    , ( "created", E.string "2021-02-25T11:33:42.052Z" )
                    ]
              )
            , ( "created", Iso8601.encode model.time )
            ]
                |> E.object
                |> Http.jsonBody
    in
    Http.post
        { url = Server.url ++ "/posts"
        , body = body
        , expect = Api.Data.expectJson options.onResponse articleDecoder
        }
