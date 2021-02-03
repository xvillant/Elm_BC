module Api.Article exposing (..)
import Json.Decode as D 


type alias Article =
    { recipeid : Int
    , title : String
    , ingredients : List String
    , recipe : String
    }


decoder : D.Decoder Article
decoder =
    D.map4 Article
        (D.field "recipeid" D.int)
        (D.field "title" D.string)
        (D.field "ingredients" (D.list D.string))
        (D.field "recipe" D.string)