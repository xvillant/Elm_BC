module Pages.Register exposing (Model, Msg, Params, page)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http exposing (..)
import Json.Decode as D exposing (..)
import Json.Encode as E exposing (..)
import Server exposing (url)
import Shared
import Spa.Document exposing (Document)
import Spa.Generated.Route as Route exposing (toString)
import Spa.Page as Page exposing (Page)
import Spa.Url as Url exposing (Url)
import Browser.Navigation as Nav exposing (Key)


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
    , key : Key
    }


init : Shared.Model -> Url Params -> ( Model, Cmd Msg )
init shared { params } =
    ( Model "" "" "" "" "" "" shared.key, Cmd.none )



-- UPDATE


type Msg
    = FirstName String
    | LastName String
    | Password String
    | PasswordAgain String
    | Email String
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
            if String.length model.firstname == 0 then
                ( { model | warning = "Enter your first name!" }, Cmd.none )

            else if String.length model.lastname == 0 then
                ( { model | warning = "Enter your last name!" }, Cmd.none )
            
            else if model.email == "" then
                ( { model | warning = "Enter your email!" }, Cmd.none )

            else if model.password == "" then
                ( { model | warning = "Enter your password!" }, Cmd.none )

            else if passwordLength model.password then
                ( { model | warning = "Password is short!" }, Cmd.none )

            else if model.passwordAgain == "" then
                ( { model | warning = "Enter your password again!" }, Cmd.none )

            else if model.password /= model.passwordAgain && String.length model.passwordAgain > 0 then
                ( { model | warning = "Passwords do not match!" }, Cmd.none )

            else
                ( { model | warning = "Loading..." }, registerUser model )

        Response response ->
            case response of
                Ok value ->
                    ( { model | warning = "Successfully registered!" },  Nav.pushUrl model.key "/login")

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
    Sub.none



-- VIEW


view : Model -> Document Msg
view model =
    { title = "Sign Up"
    , body =
        [ div [ class "centered" ]
            [ h1 [ class "title_page" ] [ text "Sign Up" ]
            , div [ class "formFieldClasses" ]
                [ input
                    [ id "firstname"
                    , type_ "text"
                    , placeholder "First Name"
                    , Html.Attributes.value model.firstname
                    , onInput FirstName
                    ]
                    []
                ]
            , div [ class "formFieldClasses" ]
                [ input
                    [ id "lastname"
                    , type_ "text"
                    , placeholder "Last Name"
                    , Html.Attributes.value model.lastname
                    , onInput LastName
                    ]
                    []
                ]
            , div [ class "formFieldClasses" ]
                [ input
                    [ id "email"
                    , type_ "email"
                    , placeholder "Email"
                    , Html.Attributes.value model.email
                    , onInput Email
                    ]
                    []
                ]
            , div [ class "formFieldClasses" ]
                [ input
                    [ id "password"
                    , type_ "password"
                    , placeholder "Password"
                    , Html.Attributes.value model.password
                    , onInput Password
                    ]
                    []
                ]
            , div [ class "formFieldClasses" ]
                [ input
                    [ id "passwordAgain"
                    , type_ "password"
                    , placeholder "Password Again"
                    , Html.Attributes.value model.passwordAgain
                    , onInput PasswordAgain
                    ]
                    []
                ]
            , div [ class "formFieldClasses" ]
                [ button [ class "submit_button", onClick Submit ] [ text "Sign Up" ] ]
            , div [ class "warning_form" ]
                [ text model.warning ]
            , div [ class "not_registered" ]
                [ a [ class "not_registered_link", href (Route.toString Route.Login) ] [ text "Have an account?" ] ]
            ]
        ]
    }


encodeUser : Model -> E.Value
encodeUser model =
    E.object
        [ ( "firstname", E.string model.firstname )
        , ( "lastname", E.string model.lastname)
        , ( "password", E.string model.password )
        , ( "email", E.string model.email )
        , ( "bio", E.string "I am new here..." )
        , ( "image", E.string "/assets/user_default.png")
        ]


registerUser : Model -> Cmd Msg
registerUser model =
    Http.post
        { url = Server.url ++ "/users"
        , body = Http.jsonBody <| encodeUser model
        , expect = Http.expectJson Response (field "accessToken" D.string)
        }


passwordLength : String -> Bool
passwordLength password =
    if String.length password < 8 then
        True

    else
        False
