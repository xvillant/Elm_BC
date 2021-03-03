module Shared exposing
    ( Flags
    , Model
    , Msg
    , init
    , subscriptions
    , update
    , view
    )

import Api.User exposing (User, userDecoder)
import Browser.Navigation as Nav exposing (Key, pushUrl)
import Components.Footer
import Components.Navbar
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Json.Decode as D
import Ports exposing (clearUser, saveUser)
import Spa.Document exposing (Document)
import Url exposing (Url)



-- INIT


type alias Flags =
    D.Value


type alias Model =
    { url : Url
    , key : Key
    , user : Maybe User
    }


init : Flags -> Url -> Key -> ( Model, Cmd Msg )
init json url key =
    let
        user =
            json |> D.decodeValue userDecoder |> Result.toMaybe
    in
    ( Model url key user
    , Cmd.none
    )



-- UPDATE


type Msg
    = SignOutSignal
    | SignInSignal User


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SignOutSignal ->
            ( { model | user = Nothing }, Cmd.batch [ Nav.pushUrl model.key "/", clearUser ] )

        SignInSignal user ->
            ( { model | user = Just user }, saveUser user )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view :
    { page : Document msg, toMsg : Msg -> msg }
    -> Model
    -> Document msg
view { page, toMsg } model =
    { title = page.title
    , body =
        [ Components.Navbar.view
            { user = model.user
            , url = model.url
            , onSignOut = toMsg SignOutSignal
            }
        , div [ class "page" ] page.body
        , Components.Footer.view
        ]
    }
