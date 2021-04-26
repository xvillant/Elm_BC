module Pages.Users exposing (Model, Msg, Params, page)

import Api.Data exposing (Data(..), expectHeader, viewFetchError)
import Api.Profile exposing (Profile, profilesDecoder)
import Browser.Dom as Dom
import Browser.Navigation exposing (pushUrl)
import Html exposing (..)
import Html.Attributes exposing (class, height, href, placeholder, src, type_, value, width)
import Html.Events exposing (onClick, onInput)
import Http
import Server exposing (url)
import Shared
import Spa.Document exposing (Document)
import Spa.Page as Page exposing (Page)
import Spa.Url exposing (Url)
import Task
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


numberUsersLimit : Int
numberUsersLimit =
    5



-- INIT


type alias Params =
    ()


type alias Model =
    { search : String
    , sorting : String
    , profiles : Data (List Profile)
    , totalCount : Int
    , paging : Int
    , token : String
    }


init : Shared.Model -> Url Params -> ( Model, Cmd Msg )
init shared { params } =
    case shared.user of
        Just user_ ->
            ( { token = user_.token, search = "", sorting = "lastname", profiles = Loading, totalCount = 0, paging = 1 }, Cmd.batch [ getUsers user_.token 1 "" "lastname" { onResponse = ProfilesReceived }, getContentRequestHeader user_.token 1 "" "created" ] )

        Nothing ->
            ( { token = "", search = "", sorting = "lastname", profiles = Loading, totalCount = 0, paging = 1 }, pushUrl shared.key "/login" )



-- UPDATE


type Msg
    = ProfilesReceived (Data (List Profile))
    | Search String
    | ChangeSorting String
    | Tick Time.Posix
    | NoOp
    | WatchCount (Result Http.Error Int)
    | ChangePaging Int


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        ProfilesReceived profiles ->
            ( { model | profiles = profiles }, Cmd.none )

        Search searching ->
            ( { model | search = searching }, Cmd.batch [ getUsers model.token model.paging searching model.sorting { onResponse = ProfilesReceived }, getContentRequestHeader model.token model.paging searching model.sorting ] )

        ChangeSorting sorting ->
            ( { model | sorting = sorting }, Cmd.batch [ getUsers model.token model.paging model.search sorting { onResponse = ProfilesReceived }, getContentRequestHeader model.token model.paging model.search sorting ] )

        ChangePaging number ->
            ( { model | paging = number }, Cmd.batch [ getUsers model.token number model.search model.sorting { onResponse = ProfilesReceived }, getContentRequestHeader model.token number model.search model.sorting, resetViewport ] )

        Tick time ->
            ( model, Cmd.batch [ getUsers model.token model.paging model.search model.sorting { onResponse = ProfilesReceived }, getContentRequestHeader model.token model.paging model.search model.sorting ] )

        WatchCount resp ->
            case resp of
                Ok value ->
                    ( { model | totalCount = value }, Cmd.none )

                Err _ ->
                    ( model, Cmd.none )


save : Model -> Shared.Model -> Shared.Model
save model shared =
    shared


load : Shared.Model -> Model -> ( Model, Cmd Msg )
load shared model =
    ( model, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    Time.every 30000 Tick



-- VIEW


view : Model -> Document Msg
view model =
    { title = "Users | GoodFood"
    , body = [ viewProfiles model ]
    }


getUsers : String -> Int -> String -> String -> { onResponse : Data (List Profile) -> Msg } -> Cmd Msg
getUsers tokenString paging searched sorting options =
    let
        listQ =
            makeListOfQueries searched
    in
    Http.request
        { method = "GET"
        , headers = [ Http.header "Authorization" ("Bearer " ++ tokenString) ]
        , url = url ++ "/users?_sort=" ++ sorting ++ "&_order=asc&q=" ++ String.join "&q=" listQ ++ "&_page=" ++ String.fromInt paging ++ "&_limit=" ++ String.fromInt numberUsersLimit
        , body = Http.emptyBody
        , expect = Api.Data.expectJson options.onResponse profilesDecoder
        , timeout = Nothing
        , tracker = Nothing
        }


makeListOfQueries : String -> List String
makeListOfQueries searched =
    String.split " " searched |> makeClear


makeClear : List String -> List String
makeClear lst =
    List.map (\l -> String.trim l) lst


viewProfiles : Model -> Html Msg
viewProfiles model =
    case model.profiles of
        NotAsked ->
            text ""

        Loading ->
            div []
                [ img [ src "/assets/loading.gif" ] [] ]

        Success actualProfiles ->
            div []
                [ h1 [] [ text "Users" ]
                , button
                    [ case model.sorting of
                        "lastname" ->
                            class "active_category_button"

                        _ ->
                            class "category_button"
                    , onClick <| ChangeSorting "lastname"
                    ]
                    [ text "Sort by last name" ]
                , button
                    [ case model.sorting of
                        "email" ->
                            class "active_category_button"

                        _ ->
                            class "category_button"
                    , onClick <| ChangeSorting "email"
                    ]
                    [ text "Sort by email" ]
                , br [] []
                , input [ class "search_input", type_ "search", placeholder "Search...", onInput Search, value model.search ] []
                , div [ class "line_after_recipes" ] []
                , if List.isEmpty actualProfiles then
                    div [ class "profiles_list" ]
                        [ br [] []
                        , p [ class "err" ] [ text "No profiles yet..." ]
                        ]

                  else
                    div []
                        [ div [ class "profiles_list" ]
                            (List.map viewProfile actualProfiles)
                        , div []
                            (List.range 1
                                (if modBy numberUsersLimit model.totalCount == 0 then
                                    model.totalCount // numberUsersLimit

                                 else
                                    (model.totalCount // numberUsersLimit) + 1
                                )
                                |> List.map (viewPages model)
                            )
                        ]
                ]

        Failure failures ->
            viewFetchError "users" failures


viewPages : Model -> Int -> Html Msg
viewPages model number =
    button
        [ if model.paging == number then
            class "page_numbers_active"

          else
            class "page_numbers"
        , onClick (ChangePaging number)
        ]
        [ text <| String.fromInt number ]


viewProfile : Profile -> Html Msg
viewProfile profile =
    ul [ class "profile_list" ]
        [ a [ href ("/profile/" ++ String.fromInt profile.id), class "link__name" ] [ text (profile.firstname ++ " " ++ profile.lastname) ]
        , br [] []
        , a [ href ("/profile/" ++ String.fromInt profile.id) ] [ img [ class "profile__image_users", src profile.image, width 100, height 100 ] [] ]
        , a [ href ("/profile/" ++ String.fromInt profile.id) ] [ p [ class "value" ] [ text profile.email ] ]
        , div [ class "line_after_recipes" ] []
        ]


resetViewport : Cmd Msg
resetViewport =
    Task.perform (\_ -> NoOp) (Dom.setViewport 0 0)


getContentRequestHeader : String -> Int -> String -> String -> Cmd Msg
getContentRequestHeader tokenString paging searched sorting =
    let
        listQ =
            makeListOfQueries searched
    in
    Http.request
        { method = "GET"
        , headers = [ Http.header "Authorization" ("Bearer " ++ tokenString) ]
        , url = url ++ "/users?_sort=" ++ sorting ++ "&_order=asc&q=" ++ String.join "&q=" listQ ++ "&_page=" ++ String.fromInt paging ++ "&_limit=" ++ String.fromInt numberUsersLimit
        , body = Http.emptyBody
        , expect = expectHeader WatchCount
        , timeout = Nothing
        , tracker = Nothing
        }
