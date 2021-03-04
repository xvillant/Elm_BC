module Components.Carousel exposing (Model, Msg, init, subscriptions, update, view)

import Array exposing (..)
import Browser
import FeatherIcons as Icons
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Server
import Task
import Time exposing (..)



--Model
--source: array of filepaths to images that carousel displays
--current: index of currently displayed image within carousel
--dir: direction in which the carousel is moving
--counter: used to reset subscription after user interacts with carousel


type alias Model =
    { source : Array String
    , current : Int
    , dir : Direction
    , counter : Count
    }



--direction of movement


type Direction
    = Right
    | Left



--used to reset subscription


type Count
    = Stop
    | Start



--imgs contain images (paths) carousel should display


init : Array String -> Model
init imgs =
    { source = imgs
    , current = 0
    , dir = Right
    , counter = Start
    }



--Update


type Msg
    = SwitchRight Int
    | SwitchLeft Int
    | Jump Int
    | Reset


update : Msg -> Model -> Model
update msg model =
    case msg of
        SwitchRight i ->
            --start moving Left -> Right and stop subscription
            { model
                | current = handle (i + 1) model
                , dir = Right
                , counter = Stop
            }

        SwitchLeft i ->
            --start moving Left <- Right and stop sub
            { model
                | current = handle (i - 1) model
                , dir = Left
                , counter = Stop
            }

        Jump i ->
            --jump to a specific image withing carousel and stop sub
            { model | current = i, counter = Stop }

        Reset ->
            --start sub again!
            { model | counter = Start }


handle : Int -> Model -> Int
handle current model =
    --handle transition from last image to first and vice-versa
    if current < 0 then
        Array.length model.source - 1

    else if current >= Array.length model.source then
        0

    else
        current



--View


view : Model -> Html Msg
view model =
    case Array.get model.current model.source of
        Just img ->
            let
                --carousel is using background-image so this format of url is required
                url =
                    "url(" ++ img ++ ")"
            in
            div
                [ class "container-fluid text-center image"
                , style "height" "400px"
                , style "width" "100%"
                , style "background-image" url
                , style "background-color" "gray"
                , style "background-size" "cover"
                , style "overflow" "hidden"
                , style "transition" "all .5s ease-in-out"
                ]
                [ div [ style "margin-top" "150px" ]
                    [ button
                        [ class "next__button"
                        --, style "transition" "opacity 0.3s ease 0s"
                        , style "background" "Transparent"
                        , style "border" "none"
                        , style "color" "#24252a"
                        , style "position" "absolute"
                        , style "left" "50px"
                        , style "opacity" "0.5"
                        , style "outline" "none"

                        --switch direction of movement to Left if we press the left arrow
                        , onClick (SwitchLeft model.current)
                        ]
                        [ Icons.chevronLeft |> Icons.withSize 80 |> Icons.withStrokeWidth 3 |> Icons.toHtml []
                        ]
                    , button
                        [ class "next__button"
                        --, style "transition" "background 0.3s ease 0s"
                        , style "background" "transparent"
                        , style "border" "none"
                        , style "color" "#24252a"
                        , style "position" "absolute"
                        , style "right" "50px"
                        , style "opacity" "0.5"
                        , style "outline" "none"

                        --switch direction of movement to Right if we press the right arrow
                        , onClick (SwitchRight model.current)
                        ]
                        [ Icons.chevronRight |> Icons.withSize 80 |> Icons.withStrokeWidth 3 |> Icons.toHtml []
                        ]
                    ]

                --display bullets (the small dots) which allow jumping to a specific image in carousel
                , div [ style "margin-top" "350px" ] (Array.toList (Array.indexedMap (viewBullet model) model.source))
                ]

        Nothing ->
            div [] []


viewBullet : Model -> Int -> String -> Html Msg
viewBullet model index string =
    {--
    display for bullet, using indexedMap so we can Jump 
    to the index that the bullet belongs to. Also we differentiate 
    the bullet appearance based on our current index! (bullet looks different
    if our carousel's current index is the same sa bullet index)
  --}
    button
        [ style "outline" "none"
        , style "border" "none"
        , style "background" "Transparent"
        , style "opacity" "0.9"
        , style "color" "white"
        , style "position" "relative"
        , class "preview"
        , onClick (Jump index)
        ]
        [ if model.current == index then
            Icons.xCircle |> Icons.withSize 20 |> Icons.withStrokeWidth 3 |> Icons.toHtml []

          else
            Icons.circle |> Icons.withSize 20 |> Icons.withStrokeWidth 3 |> Icons.toHtml []
        ]


subscriptions : Model -> Sub Msg
subscriptions model =
    --every 5 seconds, move carousel in certain direction
    case model.counter of
        Start ->
            Time.every 5000
                (\_ ->
                    case model.dir of
                        Right ->
                            SwitchRight model.current

                        Left ->
                            SwitchLeft model.current
                )

        Stop ->
            {--
        if we stop the sub, we reset it after 1 second elapses
        this approach allows us to restart the subscription based
        on the actions of the user!
      --}
            Time.every 1000 (\_ -> Reset)


--Copied from Juraj Bedej's Bachelor thesis