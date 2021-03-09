module Pages.Settings exposing (Model, Msg, Params, page)

import Api.Data exposing (Data(..))
import Api.User exposing (User, userDecoder)
import Browser.Navigation exposing (Key, pushUrl)
import Components.Validity exposing (isValidEmail, isValidPassword)
import Html exposing (..)
import Html.Attributes exposing (class, cols, id, placeholder, rows, type_, value)
import Html.Events exposing (onClick, onInput)
import Http exposing (..)
import Iso8601
import Json.Encode as E exposing (..)
import Ports exposing (saveUser)
import Server exposing (url)
import Shared
import Spa.Document exposing (Document)
import Spa.Page as Page exposing (Page)
import Spa.Url exposing (Url)
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
    { id : Int
    , email : String
    , image : String
    , bio : String
    , firstname : String
    , lastname : String
    , password : String
    , created : Time.Posix
    , warning : String
    , key : Key
    , user : Maybe User
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
            , password = ""
            , id = user.id
            , created = user.created
            , warning = ""
            , key = shared.key
            , user = shared.user
            }

        Nothing ->
            { image = ""
            , firstname = ""
            , lastname = ""
            , bio = ""
            , email = ""
            , id = 0
            , password = ""
            , warning = ""
            , created = Time.millisToPosix 0
            , key = shared.key
            , user = Nothing
            }
    , Cmd.none
    )



-- UPDATE


type Msg
    = Email String
    | Image String
    | Password String
    | Bio String
    | SubmitUpdate
    | Updated (Data User)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Email email ->
            ( { model | email = email, warning = "" }, Cmd.none )

        Password password ->
            ( { model | password = password, warning = "" }, Cmd.none )

        Bio bio ->
            ( { model | bio = bio, warning = "" }, Cmd.none )

        Image image ->
            ( { model | image = image, warning = "" }, Cmd.none )

        SubmitUpdate ->
            if String.isEmpty model.firstname then
                ( { model | warning = "Type your first name!" }, Cmd.none )

            else if String.isEmpty model.lastname then
                ( { model | warning = "Type your last name!" }, Cmd.none )

            else if String.isEmpty model.email then
                ( { model | warning = "Type your email!" }, Cmd.none )

            else if isValidEmail model.email /= True then
                ( { model | warning = "Enter a valid email!" }, Cmd.none )

            else if String.isEmpty model.image then
                ( { model | warning = "Type your image URL!" }, Cmd.none )

            else if String.isEmpty model.password then
                ( { model | warning = "Type your password!" }, Cmd.none )

            else if isValidPassword model.password /= True then
                ( { model | warning = "Password must contains of at least - one uppercase letter, one lowercase letter, one digit, one special character and must have minimum eight in lenght" }, Cmd.none )

            else
                ( { model | warning = "Loading..." }, updateProfile model { onResponse = Updated } )

        Updated user ->
            case user of
                Api.Data.Success user_ ->
                    ( { model | user = Api.Data.toMaybe user }, Cmd.batch [ saveUser user_, pushUrl model.key "/" ] )

                _ ->
                    ( model, Cmd.none )


save : Model -> Shared.Model -> Shared.Model
save model shared =
    { shared | user = model.user }


load : Shared.Model -> Model -> ( Model, Cmd Msg )
load shared model =
    ( case shared.user of
        Just user ->
            { image = user.image
            , firstname = user.firstname
            , lastname = user.lastname
            , bio = user.bio
            , email = user.email
            , password = ""
            , id = user.id
            , created = user.created
            , warning = ""
            , key = shared.key
            , user = shared.user
            }

        Nothing ->
            { image = ""
            , firstname = ""
            , lastname = ""
            , bio = ""
            , email = ""
            , id = 0
            , password = ""
            , warning = ""
            , created = Time.millisToPosix 0
            , key = shared.key
            , user = Nothing
            }
    , Cmd.none
    )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Model -> Document Msg
view model =
    { title = "Settings | GoodFood"
    , body =
        [ div []
            [ h1 [] [ text "Settings" ]
            , div []
                [ input
                    [ id "email"
                    , type_ "email"
                    , placeholder "Type email"
                    , value model.email
                    , onInput Email
                    , class "form"
                    ]
                    []
                ]
            , div []
                [ input
                    [ id "image"
                    , type_ "text"
                    , placeholder "Type profile picture URL"
                    , value model.image
                    , onInput Image
                    , class "form"
                    ]
                    []
                ]
            , div []
                [ input
                    [ id "password"
                    , type_ "password"
                    , placeholder "Type your new password"
                    , value model.password
                    , onInput Password
                    , class "form"
                    ]
                    []
                ]
            , div []
                [ textarea
                    [ id "bio"
                    , placeholder "Type your bio"
                    , value model.bio
                    , onInput Bio
                    , rows 10
                    , cols 70
                    , class "form"
                    ]
                    []
                ]
            , div []
                [ button [ class "submit_button", onClick SubmitUpdate ] [ text "Save settings" ] ]
            , div [ class "warning_form" ]
                [ text model.warning ]
            ]
        ]
    }


updateProfile : Model -> { onResponse : Data User -> Msg } -> Cmd Msg
updateProfile model options =
    Http.request
        { method = "PUT"
        , headers = []
        , url = url ++ "/users/" ++ String.fromInt model.id
        , body = Http.jsonBody <| encodeUser model
        , expect = Api.Data.expectJson options.onResponse userDecoder
        , timeout = Nothing
        , tracker = Nothing
        }


encodeUser : Model -> E.Value
encodeUser model =
    E.object
        [ ( "id", E.int model.id )
        , ( "firstname", E.string model.firstname )
        , ( "lastname", E.string model.lastname )
        , ( "password", E.string model.password )
        , ( "bio", E.string model.bio )
        , ( "created", Iso8601.encode model.created )
        , ( "email", E.string model.email )
        , ( "image", E.string model.image )
        ]
