module Typst.Typst exposing
    ( Model
    , Msg
    , init
    , subscriptions
    , update
    , view
    )

import Html exposing (Html, div, img, text, textarea)
import Html.Attributes exposing (src, value)
import Html.Events exposing (onInput)
import Typst.Port as Port
import Url


type alias Model =
    { source : String
    , result : Result String String
    }


type Msg
    = SourceChanged String
    | TypstCompiled (Result String String)


init : Model
init =
    { source = ""
    , result = Err ""
    }


view : Model -> Html Msg
view model =
    div []
        [ textarea
            [ value model.source
            , onInput SourceChanged
            ]
            []
        , div []
            [ case model.result of
                Ok svg ->
                    img [ src (svgDataUrl svg) ] []

                Err error ->
                    text error
            ]
        ]


svgDataUrl : String -> String
svgDataUrl svg =
    "data:image/svg+xml;charset=utf-8," ++ Url.percentEncode svg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SourceChanged source ->
            ( { model | source = source }
            , Port.compileTypst source
            )

        TypstCompiled result ->
            ( { model | result = result }
            , Cmd.none
            )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Port.typstCompiled TypstCompiled
