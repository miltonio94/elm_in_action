module PhotoGroove exposing (main)

import Html exposing (Html, div, h1, img, text)
import Html.Attributes as Attributes


view : Model -> Html msg
view model =
    div
        [ Attributes.class "content" ]
        [ h1 [] [ text "Photo Groove" ]
        , div [ Attributes.id "thumbnails" ] (List.map viewThumbnail model)
        ]


type alias Model =
    List ImageStub


type alias ImageStub =
    { url : String }


initialModel : Model
initialModel =
    [ { url = "1.jpeg" }
    , { url = "1.jpeg" }
    , { url = "1.jpeg" }
    ]


urlPrefix : String
urlPrefix =
    "http://elm-in-action-.com/"


viewThumbnail : ImageStub -> Html msg
viewThumbnail thumb =
    img [ Attributes.src (urlPrefix ++ thumb.url) ] []


main : Html msg
main =
    view initialModel
