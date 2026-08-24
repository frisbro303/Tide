module Main exposing (main)

import Browser
import Html exposing (Html, div)
import Typst.Typst as Typst


type alias Model =
    { typst : Typst.Model
    }


type Msg
    = TypstMsg Typst.Msg


main : Program () Model Msg
main =
    Browser.element
        { init =
            \_ ->
                ( { typst = Typst.init }
                , Cmd.none
                )
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


view : Model -> Html Msg
view model =
    div []
        [ Html.map TypstMsg (Typst.view model.typst)
        ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        TypstMsg typstMsg ->
            let
                ( typstModel, cmd ) =
                    Typst.update typstMsg model.typst
            in
            ( { model | typst = typstModel }
            , Cmd.map TypstMsg cmd
            )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.map TypstMsg (Typst.subscriptions model.typst)
