module Pages.Login exposing (Model, Msg, Params, page)

import Api.Data exposing (Data(..))
import Api.User exposing (User, userDecoder)
import Browser.Navigation as Nav exposing (Key, pushUrl)
import Html exposing (..)
import Html.Attributes exposing (class, href, id, placeholder, type_, value)
import Html.Events exposing (onClick, onInput)
import Http exposing (..)
import Json.Decode as D exposing (Decoder, field)
import Json.Decode.Extra as ED exposing (andMap)
import Json.Encode as E exposing (..)
import Jwt
import Server
import Shared
import Spa.Document exposing (Document)
import Spa.Generated.Route as Route
import Spa.Page as Page exposing (Page)
import Spa.Url as Url exposing (Url)
import Task
import Time exposing (Month(..))


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
    , tokenString : String
    , user : Data User
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
      , tokenString = ""
      , key = shared.key
      , user =
            case shared.user of
                Just user ->
                    Success user

                Nothing ->
                    NotAsked
      }
    , Cmd.none
    )



-- UPDATE


type Msg
    = Email String
    | Password String
    | Submit
    | Response (Result Http.Error String)
 --   | DecodeToken (Result Jwt.JwtError Token)
    | GotUser (Result Http.Error User)


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
                ( model, Cmd.batch [ loginRequest model ] )

        {--DecodeToken token ->
            case token of
                Ok token_ ->
                    ( { model
                        | token =
                            case token_ of
                                Just tokenS ->
                                    tokenS

                                Nothing ->
                                    Nothing
                      }
                    , Cmd.none
                    )

                Err err ->
                    ( model, Cmd.none )--}

        Response response ->
            case response of
                Ok value ->
                    ( { model | warning = "Successfully logged in!", tokenString = value }, Cmd.batch [ getUser model, pushUrl model.key "/recipes" ] )

                Err err ->
                    ( { model | warning = httpErrorString err }, Cmd.none )

        GotUser user ->
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
                    , value model.email
                    , onInput Email
                    ]
                    []
                ]
            , div [ class "formFieldClasses" ]
                [ input
                    [ id "password"
                    , type_ "password"
                    , placeholder "Type your password"
                    , value model.password
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


getUser : Model -> Cmd Msg
getUser model =
    Http.request
        { method = "GET"
        , headers = [ Http.header "Authorization" ("Bearer " ++ model.tokenString) ]
        , url =
            Server.url
                ++ "/users/"
                ++ (case model.token of
                        Nothing ->
                            "0"

                        Just token ->
                            token.userId
                   )
        , body = Http.emptyBody
        , expect = Http.expectJson GotUser userDecoder
        , timeout = Nothing
        , tracker = Nothing
        }


{--decodeJWT : Model -> Result Jwt.JwtError Token
decodeJWT model =
    Jwt.decodeToken jwtDecoder model.tokenString


jwtDecoder : Decoder Token
jwtDecoder =
    D.succeed Token
        |> andMap (field "iat" D.int)
        |> andMap (field "exp" D.int)
        |> andMap (field "sub" D.string)
        |> andMap (field "email" D.string)


decodeToken : Cmd Msg
decodeToken =
    Task.attempt DecodeToken decodeJWT--}
