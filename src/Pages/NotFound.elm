module Pages.NotFound exposing (Model, Msg, Params, page)

import Html exposing (..)
import Html.Attributes exposing (class, href)
import Spa.Document exposing (Document)
import Spa.Generated.Route as Route
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
view { params } =
    { title = "404"
    , body =
        [ div [ class "centered" ]
            [ h1 [ class "title_page" ] [ text "Page not found!" ]
            , div [ class "not_registered" ]
                [ a [ class "not_registered_link", href (Route.toString Route.Top) ] [ text "Try this link" ] ]
            ]
        ]
    }
