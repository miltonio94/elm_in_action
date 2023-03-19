module PhotoGroove exposing (main)

import Html exposing (Html, div, h1, img, text)
import Html.Attributes as Attributes


view : Model -> Html msg
view model =
    div
        [ Attributes.class "content" ]
        [ h1 [] [ text "Photo Groove" ]
        , div [ Attributes.id "thumbnails" ]
            (List.map
                (\photo -> viewThumbnail (model.selectedUrl == photo.url) photo)
                model.photos
            )
        , img
            [ Attributes.class "large"
            , Attributes.src (urlPrefix ++ "large/" ++ model.selectedUrl)
            ]
            []
        ]


type alias Model =
    { photos : List ImageStub
    , selectedUrl : String
    }


type alias ImageStub =
    { url : String }


initialModel : Model
initialModel =
    { photos =
        [ { url = "1.jpeg" }
        , { url = "2.jpeg" }
        , { url = "3.jpeg" }
        ]
    , selectedUrl = "1.jpeg"
    }


urlPrefix : String
urlPrefix =
    "http://elm-in-action.com/"


viewThumbnail : Bool -> ImageStub -> Html msg
viewThumbnail isSelected thumb =
    img
        [ Attributes.classList
            [ ( "selected", isSelected ) ]
        , Attributes.src (urlPrefix ++ thumb.url)
        ]
        []


main : Html msg
main =
    view initialModel
