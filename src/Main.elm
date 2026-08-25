module Main exposing (main)

import Browser
import Html exposing (Html, button, div, li, text, ul)
import Html.Events exposing (onClick)
import Http
import Json.Decode as Decode
import Local.Store as Store
import Ops.Op as Op exposing (Op, OpId(..), OpKind(..))
import Random
import Sync.Account as Account
import Sync.Session as Session exposing (Session)
import Sync.Sync as Sync
import Task
import Time
import Typst.Typst as Typst
import UUID


type alias Model =
    { account : Account.Model
    , session : Maybe Session
    , typst : Typst.Model
    , ops : List Op
    }


type Msg
    = AccountMsg Account.Msg
    | TypstMsg Typst.Msg
    | StoreLoaded String Decode.Value
    | FetchOpsClicked
    | GotOps (Result Http.Error (List Op))
    | AppendTestOpClicked
    | GotTimeForNewOp Time.Posix
    | GotAppendResult (Result Http.Error ())


main : Program () Model Msg
main =
    Browser.element
        { init =
            \_ ->
                ( { account = Account.init
                  , session = Nothing
                  , typst = Typst.init
                  , ops = []
                  }
                , Session.request
                )
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        AccountMsg accountMsg ->
            let
                ( accountModel, cmd, sessionUpdate ) =
                    Account.update accountMsg model.account

                ( newSession, persistCmd ) =
                    case sessionUpdate of
                        Account.SessionEstablished session ->
                            ( Just session, Session.save session )

                        Account.SessionCleared ->
                            ( Nothing, Session.clear )

                        Account.NoSessionChange ->
                            ( model.session, Cmd.none )
            in
            ( { model | account = accountModel, session = newSession }
            , Cmd.batch [ Cmd.map AccountMsg cmd, persistCmd ]
            )

        TypstMsg typstMsg ->
            let
                ( typstModel, cmd ) =
                    Typst.update typstMsg model.typst
            in
            ( { model | typst = typstModel }
            , Cmd.map TypstMsg cmd
            )

        StoreLoaded loadedKey value ->
            case Session.decodeFromStore loadedKey value of
                Just session ->
                    ( model, Cmd.map AccountMsg (Account.refresh session.refreshToken) )

                Nothing ->
                    ( model, Cmd.none )

        FetchOpsClicked ->
            case model.session of
                Just session ->
                    ( model, Sync.fetchOps session GotOps )

                Nothing ->
                    ( model, Cmd.none )

        GotOps (Ok ops) ->
            ( { model | ops = ops }, Cmd.none )

        GotOps (Err _) ->
            ( model, Cmd.none )

        AppendTestOpClicked ->
            ( model, Task.perform GotTimeForNewOp Time.now )

        GotTimeForNewOp now ->
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
                            , front = "Test front"
                            , back = "Test back"
                            }
                    }
            in
            case model.session of
                Just session ->
                    ( model, Sync.appendOp session op GotAppendResult )

                Nothing ->
                    ( model, Cmd.none )

        GotAppendResult _ ->
            ( model, Cmd.none )


view : Model -> Html Msg
view model =
    div []
        [ Html.map AccountMsg (Account.view model.session model.account)
        , case model.session of
            Just _ ->
                div []
                    [ button [ onClick AppendTestOpClicked ] [ text "Append test op" ]
                    , button [ onClick FetchOpsClicked ] [ text "Fetch ops" ]
                    , ul [] (List.map (\_ -> li [] [ text "op" ]) model.ops)
                    ]

            Nothing ->
                text ""
        , Html.map TypstMsg (Typst.view model.typst)
        ]


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Sub.map TypstMsg (Typst.subscriptions model.typst)
        , Store.loaded StoreLoaded
        ]
