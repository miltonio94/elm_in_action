module PhotoGroove exposing (main)

import Array exposing (Array)
import Browser
import Html exposing (Html, div, h1, img, text)
import Html.Attributes as Attributes
import Html.Events as Events
import Http
import Json.Decode as Json
import Json.Decode.Pipeline as JsonPipeline
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


photoDecoder : Json.Decoder Photo
photoDecoder =
    Json.succeed Photo
        |> JsonPipeline.required "url" Json.string
        |> JsonPipeline.required "size" Json.int
        |> JsonPipeline.optional "title" Json.string "(untitled)"


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
        , expect = Http.expectJson GotPhotos (Json.list photoDecoder)
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


main : Program () Model Msg
main =
    Browser.element
        { init = \_ -> ( initialModel, initialCommand )
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }
