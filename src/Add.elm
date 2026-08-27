module Add exposing (Model, Msg, OutMsg(..), editBack, editFront, init, subscriptions, update, view)

import Html exposing (Html, button, div, hr, p, text)
import Html.Attributes exposing (class, id)
import Html.Events exposing (onClick)
import Ops.Op exposing (Op, OpId(..), OpKind(..))
import Random
import Task
import Time
import Typst.EditableTypst as EditableTypst
import UUID


type alias Model =
    { front : EditableTypst.Model
    , back : EditableTypst.Model
    , preamble : String
    , error : Maybe String
    }


init : String -> Model
init preamble =
    { front = EditableTypst.init "add-front" "Front" "i" preamble
    , back = EditableTypst.init "add-back" "Back" "o" preamble
    , preamble = preamble
    , error = Nothing
    }


type Msg
    = FrontMsg EditableTypst.Msg
    | BackMsg EditableTypst.Msg
    | SubmitClicked
    | GotTimeForSubmit Time.Posix
    | CancelClicked


type OutMsg
    = NoOutMsg
    | Submitted Op
    | Canceled


editFront : Msg
editFront =
    FrontMsg EditableTypst.requestFocus


editBack : Msg
editBack =
    BackMsg EditableTypst.requestFocus


update : Msg -> Model -> ( Model, Cmd Msg, OutMsg )
update msg model =
    case msg of
        FrontMsg frontMsg ->
            let
                ( frontModel, cmd, _ ) =
                    EditableTypst.update frontMsg model.front
            in
            ( { model | front = frontModel }, Cmd.map FrontMsg cmd, NoOutMsg )

        BackMsg backMsg ->
            let
                ( backModel, cmd, _ ) =
                    EditableTypst.update backMsg model.back
            in
            ( { model | back = backModel }, Cmd.map BackMsg cmd, NoOutMsg )

        SubmitClicked ->
            if EditableTypst.isBlank model.front || EditableTypst.isBlank model.back then
                ( { model | error = Just "Both sides are required" }, Cmd.none, NoOutMsg )

            else
                ( { model | error = Nothing }, Task.perform GotTimeForSubmit Time.now, NoOutMsg )

        GotTimeForSubmit now ->
            let
                ( cardUuid, seed1 ) =
                    Random.step UUID.generator (Random.initialSeed (Time.posixToMillis now))

                ( opUuid, _ ) =
                    Random.step UUID.generator seed1

                op =
                    { id = OpId opUuid
                    , timeStamp = now
                    , opKind =
                        CreateCard
                            { id = cardUuid
                            , front = EditableTypst.currentSource model.front
                            , back = EditableTypst.currentSource model.back
                            }
                    }
            in
            ( init model.preamble, Cmd.none, Submitted op )

        CancelClicked ->
            ( init model.preamble, Cmd.none, Canceled )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Sub.map FrontMsg (EditableTypst.subscriptions model.front)
        , Sub.map BackMsg (EditableTypst.subscriptions model.back)
        ]


view : Model -> Html Msg
view model =
    div [ class "review-card-area" ]
        [ Html.map FrontMsg (EditableTypst.view model.front)
        , hr [ class "review-divider" ] []
        , Html.map BackMsg (EditableTypst.view model.back)
        , case model.error of
            Just err ->
                p [ class "note-editor-error" ] [ text err ]

            Nothing ->
                text ""
        , div [ class "review-actions" ]
            [ button [ id "add-submit-button", class "button-primary", onClick SubmitClicked ] [ text "Add" ]
            , button [ class "button-ghost", onClick CancelClicked ] [ text "Cancel" ]
            ]
        ]
