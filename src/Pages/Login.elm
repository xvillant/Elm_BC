module Pages.Login exposing (Model, Msg, Params, page)

import Api.Data exposing (Data(..))
import Api.Token exposing (decodeJWT)
import Api.User exposing (User, userDecoder)
import Browser.Navigation exposing (Key, pushUrl)
import Html exposing (..)
import Html.Attributes exposing (class, href, id, placeholder, type_, value)
import Html.Events exposing (onClick, onInput)
import Http exposing (..)
import Json.Decode as D exposing (field)
import Json.Encode as E exposing (..)
import Ports exposing (saveUser)
import Server
import Shared
import Spa.Document exposing (Document)
import Spa.Generated.Route as Route
import Spa.Page as Page exposing (Page)
import Spa.Url exposing (Url)


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
    , user : Maybe User
    , token : String
    }


init : Shared.Model -> Url Params -> ( Model, Cmd Msg )
init shared { params } =
    case shared.user of
        Just u ->
            ( { email = ""
              , password = ""
              , warning = ""
              , key = shared.key
              , user = shared.user
              , token = ""
              }
            , pushUrl shared.key "/"
            )

        Nothing ->
            ( { email = ""
              , password = ""
              , warning = ""
              , key = shared.key
              , user = Nothing
              , token = ""
              }
            , Cmd.none
            )



-- UPDATE


type Msg
    = Email String
    | Password String
    | Submit
    | Response (Result Http.Error String)
    | GotUser (Data User)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Email email ->
            ( { model | email = email, warning = "" }, Cmd.none )

        Password password ->
            ( { model | password = password, warning = "" }, Cmd.none )

        Submit ->
            if String.isEmpty model.email then
                ( { model | warning = "Type your email!" }, Cmd.none )

            else if String.isEmpty model.password then
                ( { model | warning = "Type your password!" }, Cmd.none )

            else
                ( { model | warning = "Loading..." }, loginRequest model )

        Response response ->
            case response of
                Ok value ->
                    ( { model | warning = "Loading...", token = value }, getUser value { onResponse = GotUser } )

                Err err ->
                    ( { model | warning = httpErrorString err, password = "", email = "" }, Cmd.none )

        GotUser user ->
            case user of
                Api.Data.Success user_ ->
                    ( { model
                        | user =
                            case Api.Data.toMaybe user of
                                Just u ->
                                    Just { u | token = model.token }

                                _ ->
                                    Nothing
                      }
                    , Cmd.batch [ saveUser { user_ | token = model.token }, pushUrl model.key "/recipes" ]
                    )

                Failure f ->
                    ( { model
                        | warning =
                            case List.head f of
                                Just a ->
                                    a

                                Nothing ->
                                    ""
                      }
                    , Cmd.none
                    )

                _ ->
                    ( { model | warning = "Something went wrong!" }, Cmd.none )


httpErrorString : Http.Error -> String
httpErrorString error =
    case error of
        BadUrl text ->
            "Bad Url: " ++ text

        Timeout ->
            "Http Timeout"

        NetworkError ->
            "Connection issues"

        BadStatus response ->
            case response of
                400 ->
                    "Invalid email or password!"

                _ ->
                    "Something went wrong!"

        _ ->
            "Something went wrong!"


save : Model -> Shared.Model -> Shared.Model
save model shared =
    { shared | user = model.user }


load : Shared.Model -> Model -> ( Model, Cmd Msg )
load shared model =
    ( model, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Model -> Document Msg
view model =
    { title = "Sign In | GoodFood"
    , body =
        [ div []
            [ h1 [] [ text "Sign In" ]
            , div []
                [ input
                    [ id "email"
                    , type_ "email"
                    , placeholder "Type your email"
                    , value model.email
                    , onInput Email
                    , class "form"
                    ]
                    []
                ]
            , div []
                [ input
                    [ id "password"
                    , type_ "password"
                    , placeholder "Type your password"
                    , value model.password
                    , onInput Password
                    , class "form"
                    ]
                    []
                ]
            , div []
                [ button [ class "submit_button", onClick Submit ] [ text "Sign In" ] ]
            , div [ class "warning_form" ]
                [ text model.warning ]
            , div []
                [ br [] []
                , a [ class "link", href (Route.toString Route.Register) ] [ text "Don't have an account?" ]
                ]
            ]
        ]
    }


loginRequest : Model -> Cmd Msg
loginRequest model =
    let
        body =
            [ ( "email", E.string model.email )
            , ( "password", E.string model.password )
            ]
                |> E.object
                |> Http.jsonBody
    in
    Http.request
        { method = "POST"
        , headers = []
        , url = Server.url ++ "/login"
        , body = body
        , expect = Http.expectJson Response (field "accessToken" D.string)
        , timeout = Nothing
        , tracker = Nothing
        }


getUser : String -> { onResponse : Data User -> Msg } -> Cmd Msg
getUser tokenString options =
    let
        token =
            decodeJWT tokenString
    in
    Http.request
        { method = "GET"
        , headers = [ Http.header "Authorization" ("Bearer " ++ tokenString) ]
        , url =
            Server.url
                ++ "/users/"
                ++ (case token of
                        Err err ->
                            "0"

                        Ok token_ ->
                            token_.userId
                   )
        , body = Http.emptyBody
        , expect = Api.Data.expectJson options.onResponse userDecoder
        , timeout = Nothing
        , tracker = Nothing
        }
