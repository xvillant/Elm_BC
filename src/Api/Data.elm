module Api.Data exposing
    ( Data(..)
    , expectJson
    , map
    , toMaybe
    , viewFetchError
    , expectHeader
    )

import Http
import Json.Decode as D
import Html exposing (..)
import Html.Attributes exposing (class)
import Dict


type Data value
    = NotAsked
    | Loading
    | Failure (List String)
    | Success value


map : (a -> b) -> Data a -> Data b
map fn data =
    case data of
        NotAsked ->
            NotAsked

        Loading ->
            Loading

        Failure reason ->
            Failure reason

        Success value ->
            Success (fn value)


toMaybe : Data value -> Maybe value
toMaybe data =
    case data of
        Success value ->
            Just value

        _ ->
            Nothing


expectJson : (Data value -> msg) -> D.Decoder value -> Http.Expect msg
expectJson toMsg decoder =
    Http.expectStringResponse (fromResult >> toMsg) <|
        \response ->
            case response of
                Http.BadUrl_ _ ->
                    Err [ "Bad URL" ]

                Http.Timeout_ ->
                    Err [ "Request timeout" ]

                Http.NetworkError_ ->
                    Err [ "Connection issues" ]

                Http.BadStatus_ _ body ->
                    case D.decodeString errorDecoder body of
                        Ok errors ->
                            Err errors

                        Err _ ->
                            Err [ "Bad status code" ]

                Http.GoodStatus_ _ body ->
                    case D.decodeString decoder body of
                        Ok value ->
                            Ok value

                        Err err ->
                            Err [ D.errorToString err ]


errorDecoder : D.Decoder (List String)
errorDecoder =
    D.keyValuePairs (D.list D.string)
        |> D.field "errors"
        |> D.map (List.concatMap (\( key, values ) -> values |> List.map (\value -> key ++ " " ++ value)))


fromResult : Result (List String) value -> Data value
fromResult result =
    case result of
        Ok value ->
            Success value

        Err reasons ->
            Failure reasons




--https://www.elm-spa.dev/guide/using-apis


viewFetchError : String -> List String -> Html msg
viewFetchError data errorMessage =
    let
        errorHeading =
            "Couldn't fetch " ++ data ++ "."
    in
    div []
        [ h1 [] [ text errorHeading ]
        , text ("Error: ")
        , ol [ class "errors" ]
            (List.map (\l -> li [ class "error__value" ] [ text l ]) errorMessage)
        ]



expectHeader : (Result Http.Error Int -> msg) -> Http.Expect msg
expectHeader toMsg =
    Http.expectStringResponse toMsg <|
        \response ->
            case response of
                Http.BadUrl_ url ->
                    Err (Http.BadUrl url)

                Http.Timeout_ ->
                    Err Http.Timeout

                Http.NetworkError_ ->
                    Err Http.NetworkError

                Http.BadStatus_ metadata body ->
                    Err (Http.BadStatus metadata.statusCode)

                Http.GoodStatus_ metadata body ->
                    case Dict.get "x-total-count" metadata.headers of
                        Just number ->
                            Ok
                                (case String.toInt <| number of
                                    Nothing ->
                                        0

                                    Just number_ ->
                                        number_
                                )

                        Nothing ->
                            Ok 0