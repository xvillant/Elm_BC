module Pages.Settings exposing (Model, Msg, Params, page)

import Html exposing (..)
import Html.Attributes exposing (value, class, id, placeholder, type_, rows, cols)
import Html.Events exposing (onInput, onClick)
import Http exposing (..)
import Server exposing (url)
import Shared
import Spa.Document exposing (Document)
import Spa.Page as Page exposing (Page)
import Spa.Url exposing (Url)
import Json.Encode as E exposing (..)
import Browser.Navigation exposing (Key, pushUrl)
import Time
import Iso8601


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
    , created : Time.Posix
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
            , id = user.id
            , created = user.created
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
            , created = Time.millisToPosix 0
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
                    ( { model | warning = "Successfully updated profile!" }, pushUrl model.key "/recipes" )

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
                    , value model.firstname
                    , onInput FirstName
                    ]
                    []
                ]
            , div [ class "formFieldClasses" ]
                [ input
                    [ id "lastname"
                    , type_ "text"
                    , placeholder "Type last name"
                    , value model.lastname
                    , onInput LastName
                    ]
                    []
                ]
            , div [ class "formFieldClasses" ]
                [ input
                    [ id "email"
                    , type_ "email"
                    , placeholder "Type email"
                    , value model.email
                    , onInput Email
                    ]
                    []
                ]
            , div [ class "formFieldClasses" ]
                [ input
                    [ id "image"
                    , type_ "text"
                    , placeholder "Type profile picture URL"
                    , value model.image
                    , onInput Image
                    ]
                    []
                ]
            , div [ class "formFieldClasses" ]
                [ textarea
                    [ id "bio"
                    , placeholder "Type your bio"
                    , value model.bio
                    , onInput Bio
                    , rows 10
                    , cols 70
                    ]
                    []
                ]
            , div [ class "formFieldClasses" ]
                [ button [ class "submit_button", onClick SubmitUpdate ] [ text "Save settings" ] ]
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
        ("created", Iso8601.encode model.created),
        ("email", E.string model.email),
        ("image", E.string model.image)
    ]