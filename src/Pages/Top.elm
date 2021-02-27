module Pages.Top exposing (Model, Msg, Params, page)

import Html exposing (..)
import Html.Attributes exposing (class, height, width, src)
import Spa.Document exposing (Document)
import Spa.Page as Page exposing (Page)
import Spa.Url exposing (Url)


type alias Params =
    ()


type alias Model =
    Url Params


type alias Msg =
    Never


page : Page Params Model Msg
page =
    Page.static
        { view = view }



-- VIEW


view : Url Params -> Document Msg
view params =
    { title = "Homepage"
    , body =
        [ div [ class "centered" ]
            [ h1 [ class "title_page" ] [ text "Recipes Home" ]
            , p [] [ text "This page was made for sharing your best meals" ]
            , img [src "/assets/recipelogo.svg", width 400, height 400][]
            ]
        ]
    }
