module PhotoGroove exposing (main)

import Array exposing (Array)
import Browser
import Html exposing (Html, div, h1, img, text)
import Html.Attributes as Attributes
import Html.Events as Events
import Random


view : Model -> Html Msg
view model =
    div
        [ Attributes.class "content" ]
        [ h1 [] [ text "Photo Groove" ]
        , Html.button [ Events.onClick SupriseMotherFucker ] [ text "Suprise me" ]
        , Html.h3 [] [ text "Thumbnail size:" ]
        , div [ Attributes.id "choose-size" ]
            (List.map viewSizeChooser [ Small, Medium, Large ])
        , div
            [ Attributes.id "thumbnails"
            , Attributes.class (sizeToString model.chosenSize)
            ]
            (List.map
                (viewThumbnail model.selectedUrl)
                model.photos
            )
        , img
            [ Attributes.class "large"
            , Attributes.src (urlPrefix ++ "large/" ++ model.selectedUrl)
            ]
            []
        ]


type alias Model =
    { photos : List Photo
    , selectedUrl : String
    , chosenSize : ThumbnailSize
    }


type alias Photo =
    { url : String }


type Msg
    = SelectImg String
    | SupriseMotherFucker
    | ClickedSize ThumbnailSize
    | GotSelectedIndex Int


type ThumbnailSize
    = Small
    | Medium
    | Large


initialModel : Model
initialModel =
    { photos =
        [ { url = "1.jpeg" }
        , { url = "2.jpeg" }
        , { url = "3.jpeg" }
        ]
    , selectedUrl = "1.jpeg"
    , chosenSize = Medium
    }


getPhotoUrl : Int -> String
getPhotoUrl index =
    photoArray
        |> Array.get index
        |> photoUrlFromMaybe


photoUrlFromMaybe : Maybe Photo -> String
photoUrlFromMaybe maybePhoto =
    case maybePhoto of
        Just photo ->
            photo.url

        Nothing ->
            ""


photoArray : Array Photo
photoArray =
    Array.fromList initialModel.photos


urlPrefix : String
urlPrefix =
    "http://elm-in-action.com/"


randomPhotoPicker : Random.Generator Int
randomPhotoPicker =
    Random.int 0 (Array.length photoArray - 1)


viewThumbnail : String -> Photo -> Html Msg
viewThumbnail selectedImage thumb =
    img
        [ Attributes.classList
            [ ( "selected", selectedImage == thumb.url ) ]
        , Attributes.src (urlPrefix ++ thumb.url)
        , Events.onClick (SelectImg thumb.url)
        ]
        []


viewSizeChooser : ThumbnailSize -> Html Msg
viewSizeChooser size =
    Html.label []
        [ Html.input
            [ Attributes.type_ "radio"
            , Attributes.name "size"
            , Events.onClick (ClickedSize size)
            ]
            []
        , text (sizeToString size)
        ]


sizeToString : ThumbnailSize -> String
sizeToString size =
    case size of
        Small ->
            "small"

        Medium ->
            "medium"

        Large ->
            "large"


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SelectImg selectedImg ->
            ( { model | selectedUrl = selectedImg }, Cmd.none )

        SupriseMotherFucker ->
            ( model, Random.generate GotSelectedIndex randomPhotoPicker )

        ClickedSize size ->
            ( { model | chosenSize = size }, Cmd.none )

        GotSelectedIndex index ->
            ( { model | selectedUrl = getPhotoUrl index }, Cmd.none )


main : Program () Model Msg
main =
    Browser.element
        { init = \_ -> ( initialModel, Cmd.none )
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }
