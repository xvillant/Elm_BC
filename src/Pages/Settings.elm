module Pages.Settings exposing (Model, Msg, Params, page)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http exposing (..)
import Server exposing (url)
import Shared
import Spa.Document exposing (Document)
import Spa.Page as Page exposing (Page)
import Spa.Url as Url exposing (Url)
import Json.Encode as E exposing (..)
import Json.Decode as D exposing (..)
import Browser.Navigation as Nav exposing (Key, pushUrl)


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
    { id : Int
    , email : String
    , image : String
    , bio : String
    , firstname : String
    , lastname : String
    , password : String
    , warning : String
    , key : Key
    }


init : Shared.Model -> Url Params -> ( Model, Cmd Msg )
init shared { params } =
    ( case shared.user of
        Just user ->
            { image = user.image
            , firstname = user.firstname
            , lastname = user.lastname
            , bio = user.bio
            , email = user.email
            , password = user.password
            , id = user.id
            , warning = ""
            , key = shared.key
            }

        Nothing ->
            { image = ""
            , firstname = ""
            , lastname = ""
            , bio = ""
            , email = ""
            , id = 0
            , warning = ""
            , password = ""
            , key = shared.key
            }
    , Cmd.none
    )



-- UPDATE


type Msg
    = FirstName String
    | LastName String
    | Email String
    | Image String
    | Bio String
    | SubmitUpdate
    | Updated (Result Http.Error String)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        FirstName firstname ->
            ( { model | firstname = firstname }, Cmd.none )

        LastName lastname ->
            ( { model | lastname = lastname }, Cmd.none )

        Email email ->
            ( { model | email = email }, Cmd.none )

        Bio bio ->
            ( { model | bio = bio }, Cmd.none )

        Image image ->
            ( { model | image = image }, Cmd.none )
        
        SubmitUpdate ->
            ( model, updateProfile model )
        
        Updated response ->
            case response of
                Ok value ->
                    ( { model | warning = "Successfully updated profile!" }, Nav.pushUrl model.key "/recipes" )

                Err err ->
                    ( { model | warning = "Something went wrong" }, Cmd.none )


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
    { title = "Settings"
    , body =
        [ div [ class "centered" ]
            [ h1 [ class "title_page" ] [ text "Settings" ]
            , div [ class "formFieldClasses" ]
                [ input
                    [ id "firstname"
                    , type_ "text"
                    , placeholder "Type first name"
                    , Html.Attributes.value model.firstname
                    , onInput FirstName
                    ]
                    []
                ]
            , div [ class "formFieldClasses" ]
                [ input
                    [ id "lastname"
                    , type_ "text"
                    , placeholder "Type last name"
                    , Html.Attributes.value model.lastname
                    , onInput LastName
                    ]
                    []
                ]
            , div [ class "formFieldClasses" ]
                [ input
                    [ id "email"
                    , type_ "email"
                    , placeholder "Type email"
                    , Html.Attributes.value model.email
                    , onInput Email
                    ]
                    []
                ]
            , div [ class "formFieldClasses" ]
                [ input
                    [ id "image"
                    , type_ "text"
                    , placeholder "Type profile picture URL"
                    , Html.Attributes.value model.image
                    , onInput Image
                    ]
                    []
                ]
            , div [ class "formFieldClasses" ]
                [ input
                    [ id "bio"
                    , type_ "text"
                    , placeholder "Type your bio"
                    , Html.Attributes.value model.bio
                    , onInput Bio
                    ]
                    []
                ]
            , div [ class "formFieldClasses" ]
                [ button [ class "submit_button" ] [ text "Save settings" ] ]
            , div [ class "warning_form" ]
                [ text model.warning ]
            ]
        ]
    }


updateProfile : Model -> Cmd Msg
updateProfile model =
  Http.request
    { method = "PUT"
    , headers = []
    , url = Server.url ++ "/users/" ++ String.fromInt model.id
    , body = Http.jsonBody <| encodeUser model
    , expect = Http.expectString Updated
    , timeout = Nothing
    , tracker = Nothing
    }

encodeUser : Model -> E.Value
encodeUser model = 
    E.object [
        ("id", E.int model.id),
        ("firstname", E.string model.firstname),
        ("lastname", E.string model.lastname),
        ("bio", E.string model.bio),
        ("password", E.string model.password),
        ("email", E.string model.email),
        ("image", E.string model.image)
    ]