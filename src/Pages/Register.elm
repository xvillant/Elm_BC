module Pages.Register exposing (Model, Msg, Params, page)

import Browser.Navigation as Nav exposing (Key)
import Html exposing (a, button, div, h1, input, text, br)
import Html.Attributes exposing (class, href, id, placeholder, type_, value)
import Html.Events exposing (onClick, onInput)
import Http exposing (..)
import Iso8601
import Json.Decode as D exposing (field)
import Json.Encode as E exposing (..)
import Server exposing (url)
import Shared
import Spa.Document exposing (Document)
import Spa.Generated.Route as Route exposing (toString)
import Spa.Page as Page exposing (Page)
import Spa.Url as Url exposing (Url)
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
    { email : String
    , password : String
    , passwordAgain : String
    , firstname : String
    , lastname : String
    , warning : String
    , time : Time.Posix
    , key : Key
    }


init : Shared.Model -> Url Params -> ( Model, Cmd Msg )
init shared { params } =
    ( Model "" "" "" "" "" "" (Time.millisToPosix 0) shared.key, Cmd.none )



-- UPDATE


type Msg
    = FirstName String
    | LastName String
    | Password String
    | PasswordAgain String
    | Email String
    | GetTime Time.Posix
    | Submit
    | Response (Result Http.Error String)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        FirstName firstname ->
            ( { model | firstname = firstname }, Cmd.none )

        LastName lastname ->
            ( { model | lastname = lastname }, Cmd.none )

        Email email ->
            ( { model | email = email }, Cmd.none )

        Password password ->
            ( { model | password = password }, Cmd.none )

        PasswordAgain password ->
            ( { model | passwordAgain = password }, Cmd.none )

        Submit ->
            if String.isEmpty model.firstname then
                ( { model | warning = "Enter your first name!" }, Cmd.none )

            else if String.isEmpty model.lastname then
                ( { model | warning = "Enter your last name!" }, Cmd.none )

            else if String.isEmpty model.email then
                ( { model | warning = "Enter your email!" }, Cmd.none )

            else if String.isEmpty model.password then
                ( { model | warning = "Enter your password!" }, Cmd.none )

            else if passwordLength model.password then
                ( { model | warning = "Password is short!" }, Cmd.none )

            else if String.isEmpty model.passwordAgain then
                ( { model | warning = "Enter your password again!" }, Cmd.none )

            else if model.password /= model.passwordAgain && String.length model.passwordAgain > 0 then
                ( { model | warning = "Passwords do not match!" }, Cmd.none )

            else
                ( { model | warning = "Loading..." }, registerUser model )

        GetTime time ->
            ( { model | time = time }, Cmd.none )

        Response response ->
            case response of
                Ok value ->
                    ( { model | warning = "Successfully registered!" }, Nav.pushUrl model.key "/login" )

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
                    "Email taken!"

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
    Time.every 1000 GetTime



-- VIEW


view : Model -> Document Msg
view model =
    { title = "Sign Up"
    , body =
        [ div [ ]
            [ h1 [ ] [ text "Sign Up" ]
            , div [  ]
                [ input
                    [ id "firstname"
                    , type_ "text"
                    , placeholder "First Name"
                    , value model.firstname
                    , onInput FirstName
                    , class "form"
                    ]
                    []
                ]
            , div [  ]
                [ input
                    [ id "lastname"
                    , type_ "text"
                    , placeholder "Last Name"
                    , value model.lastname
                    , onInput LastName
                    , class "form"
                    ]
                    []
                ]
            , div [  ]
                [ input
                    [ id "email"
                    , type_ "email"
                    , placeholder "Email"
                    , value model.email
                    , onInput Email
                    , class "form"
                    ]
                    []
                ]
            , div [  ]
                [ input
                    [ id "password"
                    , type_ "password"
                    , placeholder "Password"
                    , value model.password
                    , onInput Password
                    , class "form"
                    ]
                    []
                ]
            , div [  ]
                [ input
                    [ id "passwordAgain"
                    , type_ "password"
                    , placeholder "Password Again"
                    , value model.passwordAgain
                    , onInput PasswordAgain
                    , class "form"
                    ]
                    []
                ]
            , div [ ]
                [ button [ class "submit_button", onClick Submit ] [ text "Sign Up" ] ]
            , div [ class "warning_form" ]
                [ text model.warning ]
            , div [ ]
                [ br [] []
                    ,a [ class "link", href (Route.toString Route.Login) ] [ text "Have an account?" ] ]
            ]
        ]
    }

registerUser : Model -> Cmd Msg
registerUser model =
    let
        body =
            [ ( "firstname", E.string model.firstname )
            , ( "lastname", E.string model.lastname )
            , ( "password", E.string model.password )
            , ( "email", E.string model.email )
            , ( "bio", E.string "I am new here..." )
            , ( "image", E.string "/assets/user_default.png" )
            , ( "created", Iso8601.encode model.time )
            ]
                |> E.object
                |> Http.jsonBody
    in
    Http.post
        { url = Server.url ++ "/users"
        , body = body
        , expect = Http.expectJson Response (field "accessToken" D.string)
        }


passwordLength : String -> Bool
passwordLength password =
    if String.length password < 8 then
        True

    else
        False
