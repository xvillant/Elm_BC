module Pages.Profile exposing (Model, Msg, Params, page)

import Api.Data exposing (..)
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
    { profile : Data Profile
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
    ( { profile = Loading
      , warning = ""
      }
    , getUserRequest { onResponse = ReceivedUser }
    )



-- UPDATE


type Msg
    = ReceivedUser (Data Profile)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ReceivedUser response ->
            ( { model | profile = response }, Cmd.none )


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
        [ viewProfile model.profile
        , div [ class "warning_form" ]
            [ text model.warning ]
        ]
    }


viewProfile : Data Profile -> Html Msg
viewProfile profile =
    case profile of
        NotAsked ->
            text ""

        Loading ->
            div [ class "centered" ]
                [ img [ src "assets/loading.gif" ] [] ]

        Success value ->
            div [ class "centered" ]
                [ h1 [ class "title_page" ] [ text "Profile" ]
                , div [ class "profile_attr" ]
                    [ p [ class "profile_name" ] [ text "username: " ]
                    , p [ class "profile_name_x" ] [ text value.username ]
                    ]
                , div [ class "profile_attr" ]
                    [ p [ class "profile_name" ] [ text "email: " ]
                    , p [ class "profile_name_x" ] [ text value.email ]
                    ]
                , div [ class "profile_attr" ]
                    [ p [ class "profile_name" ] [ text "bio: " ]
                    , p [ class "profile_name_x" ] [ text value.bio ]
                    ]
                ]

        Api.Data.Failure _ ->
            viewFetchError "profile" "Something went wrong!"


getUserRequest : { onResponse : Data Profile -> Msg } -> Cmd Msg
getUserRequest options =
    Http.get
        { url = Server.url ++ "users/1"
        , expect = Api.Data.expectJson options.onResponse userDecoder
        }


userDecoder : Decoder Profile
userDecoder =
    map4 Profile
        (field "id" D.int)
        (field "username" D.string)
        (field "email" D.string)
        (field "bio" D.string)


viewFetchError : String -> String -> Html Msg
viewFetchError items errorMessage =
    let
        errorHeading =
            "Couldn't fetch " ++ items ++ "."
    in
    div [ class "centered" ]
        [ h1 [ class "title_comment" ] [ text errorHeading ]
        , text ("Error: " ++ errorMessage)
        ]
