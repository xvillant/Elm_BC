module Pages.Login exposing (Model, Msg, Params, page)

import Api.Data exposing (Data(..))
import Api.User exposing (User, userDecoder)
import Browser.Navigation exposing (Key, pushUrl)
import FeatherIcons exposing (user)
import Html exposing (..)
import Html.Attributes exposing (class, href, id, placeholder, type_, value)
import Html.Events exposing (onClick, onInput)
import Http exposing (..)
import Json.Decode as D exposing (Decoder, field)
import Json.Decode.Extra exposing (andMap)
import Json.Encode as E exposing (..)
import Jwt
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
    , token : Maybe Token
    }


type alias Token =
    { iat : Int
    , exp : Int
    , userId : String
    , email : String
    }


init : Shared.Model -> Url Params -> ( Model, Cmd Msg )
init shared { params } =
    ( { email = ""
      , password = ""
      , warning = ""
      , token = Nothing
      , key = shared.key
      , user = shared.user
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
            ( { model | email = email }, Cmd.none )

        Password password ->
            ( { model | password = password }, Cmd.none )

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
                    ( { model | warning = "Loading..." }, getUser value model { onResponse = GotUser } )

                Err err ->
                    ( { model | warning = httpErrorString err }, Cmd.none )

        GotUser user ->
            case user of
                Api.Data.Success user_ ->
                    ( { model | user = Api.Data.toMaybe user }, Cmd.batch [ saveUser user_, pushUrl model.key "/recipes" ] )

                _ ->
                    ( model, Cmd.none )


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
    { title = "Sign In"
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
    Http.post
        { url = Server.url ++ "/login"
        , body = body
        , expect = Http.expectJson Response (field "accessToken" D.string)
        }


getUser : String -> Model -> { onResponse : Data User -> Msg } -> Cmd Msg
getUser tokenString model options =
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


decodeJWT : String -> Result Jwt.JwtError Token
decodeJWT tokenString =
    Jwt.decodeToken jwtDecoder tokenString


jwtDecoder : Decoder Token
jwtDecoder =
    D.succeed Token
        |> andMap (field "iat" D.int)
        |> andMap (field "exp" D.int)
        |> andMap (field "sub" D.string)
        |> andMap (field "email" D.string)
