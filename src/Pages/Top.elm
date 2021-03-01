module Pages.Top exposing (Model, Msg, Params, page)

import Array
import Browser.Navigation as Nav exposing (Key)
import Components.Carousel as Carousel
import Html exposing (..)
import Html.Attributes exposing (..)
import Spa.Document exposing (Document)
import Spa.Page as Page exposing (Page)
import Spa.Url as Url exposing (Url)
import Time


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
    { carousel : Carousel.Model
    }


init : Url Params -> ( Model, Cmd Msg )
init { params } =
    ( Model
        (Carousel.init
            (Array.fromList
                [ "assets/caroussel/1.png"
                , "assets/caroussel/2.jpg"
                , "assets/caroussel/3.jpg"
                , "assets/caroussel/4.jpg"
                , "assets/caroussel/5.png"
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
    { title = "Home"
    , body =
        [ div [ class "centered" ]
            [ h1 [ class "title_page" ] [ text "Recipes Home" ]
            , p [] [ text "This page was made for sharing your best meals" ]
            , div [class "carrousel"] [ Carousel.view model.carousel |> Html.map UpdateCarousel ]
            ]
        ]
    }
