module Pages.Top exposing (Model, Msg, Params, page)

import Array
import Components.Carousel as Carousel
import Html exposing (..)
import Html.Attributes exposing (..)
import Spa.Document exposing (Document)
import Spa.Page as Page exposing (Page)
import Spa.Url exposing (Url)


page : Page Params Model Msg
page =
    Page.element
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }



-- INIT


type alias Params =
    ()


type alias Model =
    { carousel : Carousel.Model }


init : Url Params -> ( Model, Cmd Msg )
init { params } =
    ( Model
        (Carousel.init
            (Array.fromList
                [ "assets/caroussel/1.jpg"
                , "assets/caroussel/2.jpg"
                , "assets/caroussel/3.jpg"
                , "assets/caroussel/4.jpg"
                , "assets/caroussel/5.jpg"
                , "assets/caroussel/6.png"
                ]
            )
        )
    , Cmd.none
    )



-- UPDATE


type Msg
    = UpdateCarousel Carousel.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UpdateCarousel mesg ->
            ( { model | carousel = Carousel.update mesg model.carousel }, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    Carousel.subscriptions model.carousel |> Sub.map UpdateCarousel



-- VIEW


view : Model -> Document Msg
view model =
    { title = "Home | GoodFood"
    , body =
        [ div []
            [ h1 [] [ text "GoodFood" ]
            , div [ class "quote" ]
                [ blockquote [ class "quote__text" ] [ text "A recipe is a story that ends with a good meal." ]
                , Html.cite [] [ text "Pat Conroy" ]

                --https://codepen.io/JoeHastings/pen/MOdRVm
                ]
            , br [] []
            , div [ class "carrousel" ] [ Carousel.view model.carousel |> Html.map UpdateCarousel ]
            ]
        ]
    }
