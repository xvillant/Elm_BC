module Pages.Settings exposing (Model, Msg, Params, page)

import Api.Data exposing (Data(..))
import Api.User exposing (User, userDecoder)
import Browser.Navigation exposing (Key, pushUrl)
import Components.Image as Image exposing (Image)
import Components.Validity exposing (isValidEmail, isValidPassword)
import Html exposing (..)
import Html.Attributes exposing (accept, class, cols, id, placeholder, rows, type_, value)
import Html.Events exposing (on, onClick, onInput)
import Http exposing (..)
import Iso8601
import Json.Decode as D
import Json.Encode as E exposing (..)
import Ports exposing (ImagePortData, fileContentRead, fileSelected, saveUser)
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
    , bio : String
    , firstname : String
    , lastname : String
    , password : String
    , created : Time.Posix
    , warning : String
    , key : Key
    , user : Maybe User
    , mImage : Maybe Image
    , imageId : String
    }


init : Shared.Model -> Url Params -> ( Model, Cmd Msg )
init shared { params } =
    case shared.user of
        Just user ->
            ( { imageId = "ImageInputId"
              , mImage = Nothing
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
            , Cmd.none
            )

        Nothing ->
            ( { mImage = Nothing
              , imageId = ""
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
            , pushUrl shared.key "/login"
            )



-- UPDATE


type Msg
    = Email String
    | Password String
    | Bio String
    | SubmitUpdate
    | ImageSelected
    | ImageRead ImagePortData
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

        SubmitUpdate ->
            if String.isEmpty model.firstname then
                ( { model | warning = "Type your first name!" }, Cmd.none )

            else if String.isEmpty model.lastname then
                ( { model | warning = "Type your last name!" }, Cmd.none )

            else if String.isEmpty model.email then
                ( { model | warning = "Type your email!" }, Cmd.none )

            else if isValidEmail model.email /= True then
                ( { model | warning = "Enter a valid email!" }, Cmd.none )

            else if String.isEmpty model.password then
                ( { model | warning = "Type your password!" }, Cmd.none )

            else if isValidPassword model.password /= True then
                ( { model | warning = "Enter a valid password!" }, Cmd.none )

            else
                ( { model | warning = "Loading..." }, updateProfile model { onResponse = Updated } )

        Updated user ->
            case user of
                Api.Data.Success user_ ->
                    ( { model | user = Api.Data.toMaybe user }, Cmd.batch [ saveUser user_, pushUrl model.key "/" ] )

                _ ->
                    ( model, Cmd.none )

        ImageSelected ->
            ( model, fileSelected model.imageId )

        ImageRead data ->
            let
                newImage =
                    { contents = data.contents, filename = data.filename }
            in
            ( { model | mImage = Just newImage }
            , Cmd.none
            )


save : Model -> Shared.Model -> Shared.Model
save model shared =
    { shared | user = model.user }


load : Shared.Model -> Model -> ( Model, Cmd Msg )
load shared model =
    ( case shared.user of
        Just user ->
            { mImage = Nothing
            , imageId = "ImageInputId"
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
            { firstname = ""
            , lastname = ""
            , mImage = Nothing
            , imageId = ""
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
    fileContentRead ImageRead



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
                    [ id model.imageId
                    , type_ "file"
                    , on "change" (D.succeed ImageSelected)
                    , accept ".jpg, .png, .jpeg"
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
            , if isValidPassword model.password || String.isEmpty model.password then
                div [ class "warner" ] []

              else
                div [ class "warner" ]
                    [ p [] [ text "Password requirements:" ]
                    , ul []
                        [ li [ class "req__items" ] [ text "Must contain of at least one uppercase letter" ]
                        , li [ class "req__items" ] [ text "Must contain of at least one lowercase letter" ]
                        , li [ class "req__items" ] [ text "Must contain of at least one digit" ]
                        , li [ class "req__items" ] [ text "Must contain of at least one special character" ]
                        , li [ class "req__items" ] [ text "Password's minimum lenght is 8 characters" ]
                        ]
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
        , ( "image"
          , E.string
                (if model.mImage == Nothing then
                    case model.user of
                        Just u ->
                            u.image

                        Nothing ->
                            ""

                 else
                    case model.mImage of
                        Just i ->
                            i.contents

                        Nothing ->
                            ""
                )
          )
        ]
