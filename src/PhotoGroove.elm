module PhotoGroove exposing (main)

import Array exposing (Array)
import Browser
import Html exposing (Attribute, Html, div, h1, img, text)
import Html.Attributes as Attributes
import Html.Events as Events
import Http
import Json.Decode as Decode
import Json.Decode.Pipeline as DecodePipeline
import Json.Encode as Encode
import Random


type alias Model =
    { status : Status
    , chosenSize : ThumbnailSize
    }


type alias Photo =
    { url : String
    , size : Int
    , title : String
    }


type Msg
    = SelectImg String
    | SupriseMotherFucker
    | ClickedSize ThumbnailSize
    | GotSelectedPhoto Photo
    | GotPhotos (Result Http.Error (List Photo))


type ThumbnailSize
    = Small
    | Medium
    | Large


type Status
    = Loading
    | Loaded (List Photo) String
    | Errored String


initialModel : Model
initialModel =
    { status = Loading
    , chosenSize = Medium
    }


photoDecoder : Decode.Decoder Photo
photoDecoder =
    Decode.succeed Photo
        |> DecodePipeline.required "url" Decode.string
        |> DecodePipeline.required "size" Decode.int
        |> DecodePipeline.optional "title" Decode.string "(untitled)"


buildPhoto : String -> Int -> String -> Photo
buildPhoto url size title =
    Photo url size title


photosFromStatus : Status -> List Photo
photosFromStatus status =
    case status of
        Loaded photos _ ->
            photos

        Loading ->
            []

        Errored _ ->
            []


urlPrefix : String
urlPrefix =
    "http://elm-in-action.com/"


viewThumbnail : String -> Photo -> Html Msg
viewThumbnail selectedImage thumb =
    img
        [ Attributes.classList
            [ ( "selected", selectedImage == thumb.url ) ]
        , Attributes.src (urlPrefix ++ thumb.url)
        , Events.onClick (SelectImg thumb.url)
        , Attributes.title (thumb.title ++ " [" ++ String.fromInt thumb.size ++ " Kb]")
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


viewFilter : String -> Int -> Html Msg
viewFilter name mangnitude =
    Html.div
        [ Attributes.class "filter-slider" ]
        [ Html.label [] [ Html.text name ]
        , rangeSlider
            [ Attributes.max "11"
            , Attributes.property "val" (Encode.int mangnitude)
            ]
            []
        , Html.label [] [ text (String.fromInt mangnitude) ]
        ]


view : Model -> Html Msg
view model =
    div
        [ Attributes.class "content" ]
    <|
        case model.status of
            Loading ->
                []

            Loaded photos selected ->
                photoView photos selected model.chosenSize

            Errored err ->
                []


photoView : List Photo -> String -> ThumbnailSize -> List (Html Msg)
photoView photos selected chosenSize =
    [ h1 [] [ text "Photo Groove" ]
    , Html.button [ Events.onClick SupriseMotherFucker ] [ text "Suprise me" ]
    , div
        [ Attributes.class "filters" ]
        [ viewFilter "Hue" 0
        , viewFilter "Ripple" 0
        , viewFilter "Noise" 0
        ]
    , Html.h3 [] [ text "Thumbnail size:" ]
    , div [ Attributes.id "choose-size" ]
        (List.map viewSizeChooser [ Small, Medium, Large ])
    , div
        [ Attributes.id "thumbnails"
        , Attributes.class (sizeToString chosenSize)
        ]
        (List.map
            (viewThumbnail selected)
            photos
        )
    , img
        [ Attributes.class "large"
        , Attributes.src (urlPrefix ++ "large/" ++ selected)
        ]
        []
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


selectUrl : Status -> String -> Status
selectUrl status url =
    case status of
        Loaded photos _ ->
            Loaded photos url

        Loading ->
            status

        Errored err ->
            status


initialCommand : Cmd Msg
initialCommand =
    Http.get
        { url = "http://elm-in-action.com/photos/list.json"
        , expect = Http.expectJson GotPhotos (Decode.list photoDecoder)
        }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotPhotos (Err _) ->
            ( { model
                | status = Errored "Server error!"
              }
            , Cmd.none
            )

        GotPhotos (Ok photos) ->
            case photos of
                firstUrl :: rest ->
                    ( { model
                        | status =
                            Loaded photos firstUrl.url
                      }
                    , Cmd.none
                    )

                [] ->
                    ( { model
                        | status =
                            Errored "0 photos found!"
                      }
                    , Cmd.none
                    )

        -- Err httpError ->
        SelectImg selectedImg ->
            ( { model | status = selectUrl model.status selectedImg }, Cmd.none )

        SupriseMotherFucker ->
            case model.status of
                Loaded (firstPhoto :: otherPhotos) _ ->
                    Random.uniform firstPhoto otherPhotos
                        |> Random.generate GotSelectedPhoto
                        |> Tuple.pair model

                _ ->
                    ( model, Cmd.none )

        ClickedSize size ->
            ( { model | chosenSize = size }, Cmd.none )

        GotSelectedPhoto photo ->
            ( { model
                | status =
                    selectUrl model.status photo.url
              }
            , Cmd.none
            )


rangeSlider : List (Attribute msg) -> List (Html msg) -> Html msg
rangeSlider attributes children =
    Html.node "range-slider" attributes children


main : Program () Model Msg
main =
    Browser.element
        { init = \_ -> ( initialModel, initialCommand )
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }
