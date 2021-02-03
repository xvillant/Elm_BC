module Pages.Profile exposing (Model, Msg, Params, page)

import Html exposing (..)
import Html.Attributes exposing (..)
import Http exposing (..)
import Json.Decode as D exposing (..)
import Server exposing (url)
import Shared
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
    { profile : Profile
    , warning : String
    }


type alias Profile =
    { id : Int
    , username : String
    , email : String
    , bio : String
    }


init : Shared.Model -> Url Params -> ( Model, Cmd Msg )
init shared { params } =
    ( { profile = { id = 0, username = "", email = "", bio = "" }
      , warning = ""
      }
    , getUserRequest
    )



-- UPDATE


type Msg
    = ReceivedUser (Result Http.Error Profile)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ReceivedUser response ->
            case response of
                Ok value ->
                    ( { model | profile = value }, Cmd.none )

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
    { title = "Profile"
    , body =
        [ div [ class "centered" ]
            [ h1 [ class "title_page" ] [ text "Profile" ]
            , div [ class "profile_attr" ]
                [ p [ class "profile_name" ] [ text "username: " ]
                , p [ class "profile_name_x" ] [ text model.profile.username ]
                ]
            , div [ class "profile_attr" ]
                [ p [ class "profile_name" ] [ text "email: " ]
                , p [ class "profile_name_x" ] [ text model.profile.email ]
                ]
            , div [ class "profile_attr" ]
                [ p [ class "profile_name" ] [ text "bio: " ]
                , p [ class "profile_name_x" ] [ text model.profile.bio ]
                ]
            , div [ class "warning_form" ]
                [ text model.warning ]
            ]
        ]
    }


getUserRequest : Cmd Msg
getUserRequest =
    Http.get
        { url = Server.url ++ "users/1"
        , expect = Http.expectJson ReceivedUser userDecoder
        }


userDecoder : Decoder Profile
userDecoder =
    map4 Profile
        (field "id" D.int)
        (field "username" D.string)
        (field "email" D.string)
        (field "bio" D.string)
