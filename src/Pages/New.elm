module Pages.New exposing (Model, Msg, Params, page)

import Browser.Navigation as Nav exposing (Key, pushUrl)
import Elm.Module exposing (Name)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http exposing (..)
import Json.Decode as D exposing (..)
import Json.Encode as E exposing (..)
import Server exposing (url)
import Shared exposing (Model)
import Spa.Document exposing (Document)
import Spa.Page as Page exposing (Page)
import Spa.Url as Url exposing (Url)


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
    , key : Key
    }


init : Shared.Model -> Url Params -> ( Model, Cmd Msg )
init shared { params } =
    ( { name = "", ingredients = "", recipe = "", warning = "", key = shared.key }, Cmd.none )



-- UPDATE


type Msg
    = Name String
    | Ingredients String
    | Recipe String
    | Submit
    | Response (Result Http.Error String)


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
                ( model, postArticle model )

        Response response ->
            case response of
                Ok value ->
                    ( { model | warning = "Successfully added article!" }, Nav.pushUrl model.key "/recipes" )

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
    { title = "New Article"
    , body =
        [ div [ class "centered" ]
            [ h1 [ class "title_page" ] [ text "New Article" ]
            , div [ class "formFieldClasses" ]
                [ input
                    [ id "recipe_name"
                    , type_ "text"
                    , placeholder "Recipe Name"
                    , autocomplete False
                    , Html.Attributes.value model.name
                    , onInput Name
                    ]
                    []
                ]
            , div [ class "formFieldClasses" ]
                [ input
                    [ id "ingredients"
                    , type_ "text"
                    , autocomplete False
                    , placeholder "Ingredients (divided by ', ')"
                    , Html.Attributes.value model.ingredients
                    , onInput Ingredients
                    ]
                    []
                ]
            , div [ class "formFieldClasses" ]
                [ textarea
                    [ id "recipe"
                    , Html.Attributes.value model.recipe
                    , placeholder "Type here the recipe"
                    , onInput Recipe
                    , rows 10
                    , cols 70
                    ]
                    []
                ]
            , div [ class "formFieldClasses" ]
                [ button [ class "submit_button", onClick Submit ] [ text "Share recipe" ] ]
            , div [ class "warning_form" ]
                [ text model.warning ]
            ]
        ]
    }


encodeArticle : Model -> E.Value
encodeArticle model =
    E.object
        [ ( "name", E.string model.name )
        , ( "ingredients", E.list E.string <| String.split ", " model.ingredients )
        , ( "recipe", E.string model.recipe )
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
        , ( "userid", E.int 1 )
        ]


postArticle : Model -> Cmd Msg
postArticle model =
    Http.post
        { url = Server.url ++ "posts/"
        , body = Http.jsonBody <| encodeArticle model
        , expect = Http.expectJson Response (field "name" D.string)
        }
