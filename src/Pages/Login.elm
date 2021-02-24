module Pages.Login exposing (Model, Msg, Params, page)

import Browser.Navigation as Nav exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http exposing (..)
import Json.Decode as D exposing (..)
import Json.Encode as E exposing (..)
import Server
import Shared
import Spa.Document exposing (Document)
import Spa.Generated.Route as Route
import Spa.Page as Page exposing (Page)
import Spa.Url as Url exposing (Url)
import Api.User exposing (User)
import Api.Data exposing (Data)

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
    { email : String
    , password : String
    , warning : String
    , key : Key
    , user : Data User
    }


init : Shared.Model -> Url Params -> ( Model, Cmd Msg )
init shared { params } =
    ( { email = ""
      , password = ""
      , warning = ""
      , key = shared.key
      , user = 
      case shared.user of
            Just user ->
                Api.Data.Success user

            Nothing ->
                Api.Data.NotAsked
      }
    , Cmd.none
    )



-- UPDATE


type Msg
    = Email String
    | Password String
    | Submit
    | Response (Result Http.Error String)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Email email ->
            ( { model | email = email }, Cmd.none )

        Password password ->
            ( { model | password = password }, Cmd.none )

        Submit ->
            if String.length model.email == 0 then
                ( { model | warning = "Type your email!" }, Cmd.none )

            else if String.length model.password == 0 then
                ( { model | warning = "Type your password!" }, Cmd.none )

            else
                ( model, loginRequest model )

        Response response ->
            case response of
                Ok value ->
                    ( { model | warning = "Successfully logged in!" }, Nav.pushUrl model.key "/recipes" )

                Err err ->
                    ( { model | warning = httpErrorString err }, Cmd.none )


httpErrorString : Http.Error -> String
httpErrorString error =
    case error of
        BadUrl text ->
            "Bad Url: " ++ text

        Timeout ->
            "Http Timeout"

        NetworkError ->
            "Network Error"

        BadStatus response ->
            case response of
                400 ->
                    "Wrong email or password!"

                _ ->
                    "Something went wrong!"

        _ ->
            "Something went wrong!"


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
    { title = "Sign In"
    , body =
        [ div [ class "centered" ]
            [ h1 [ class "title_page" ] [ text "Sign In" ]
            , div [ class "formFieldClasses" ]
                [ input
                    [ id "email"
                    , type_ "email"
                    , placeholder "Type your email"
                    , Html.Attributes.value model.email
                    , onInput Email
                    ]
                    []
                ]
            , div [ class "formFieldClasses" ]
                [ input
                    [ id "password"
                    , type_ "password"
                    , placeholder "Type your password"
                    , Html.Attributes.value model.password
                    , onInput Password
                    ]
                    []
                ]
            , div [ class "formFieldClasses" ]
                [ button [ class "submit_button", onClick Submit ] [ text "Log In" ] ]
            , div [ class "warning_form" ]
                [ text model.warning ]
            , div [ class "not_registered" ]
                [ a [ class "not_registered_link", href (Route.toString Route.Register) ] [ text "Don't have an account?" ] ]
            ]
        ]
    }


loginRequest : Model -> Cmd Msg
loginRequest model =
    Http.post
        { url = Server.url ++ "/login"
        , body = Http.jsonBody <| encodeLogin model
        , expect = Http.expectJson Response (field "accessToken" D.string)
        }


encodeLogin : Model -> E.Value
encodeLogin model =
    E.object
        [ ( "email", E.string model.email )
        , ( "password", E.string model.password )
        ]
