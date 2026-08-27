module Sync.LocalOps exposing (Model, Msg(..), init, insertNewOp, sessionCleared, sessionEstablished, subscriptions, update)

import Http
import Local.Db as Db
import Ops.Op exposing (Op)
import Ops.OpsLog as OpsLog exposing (OpsLog)
import Sync.Session exposing (Session)
import Sync.Sync as Sync
import Time


type alias Model =
    OpsLog


type Msg
    = LocalOpsLoaded OpsLog
    | SyncTick Time.Posix
    | GotRemoteOps (Result Http.Error OpsLog)
    | GotPushResult (Result Http.Error ())
    | ImportedOps OpsLog


syncIntervalMs : Float
syncIntervalMs =
    15000


init : ( Model, Cmd Msg )
init =
    ( OpsLog.emptyOpsLog, Db.requestOps )


insertNewOp : Op -> Model -> ( Model, Cmd Msg )
insertNewOp op model =
    ( OpsLog.insert op model, Db.insertOp op )


sessionEstablished : Session -> Cmd Msg
sessionEstablished session =
    Sync.fetchOps session GotRemoteOps


sessionCleared : ( Model, Cmd Msg )
sessionCleared =
    ( OpsLog.emptyOpsLog, Db.clearOps )


update : Maybe Session -> Msg -> Model -> ( Model, Cmd Msg )
update session msg model =
    case msg of
        LocalOpsLoaded ops ->
            ( OpsLog.merge ops model, Cmd.none )

        SyncTick _ ->
            ( model, requestSync session )

        GotRemoteOps (Ok remoteOps) ->
            case session of
                Just activeSession ->
                    let
                        toPull =
                            OpsLog.diff remoteOps model

                        toPush =
                            OpsLog.diff model remoteOps
                    in
                    ( OpsLog.merge remoteOps model
                    , Cmd.batch
                        [ Db.insertOps toPull
                        , Sync.appendOps activeSession toPush GotPushResult
                        ]
                    )

                Nothing ->
                    ( model, Cmd.none )

        GotRemoteOps (Err _) ->
            ( model, Cmd.none )

        GotPushResult _ ->
            ( model, Cmd.none )

        ImportedOps importedOps ->
            let
                toInsert =
                    OpsLog.diff importedOps model
            in
            ( OpsLog.merge importedOps model, Db.insertOps toInsert )


requestSync : Maybe Session -> Cmd Msg
requestSync maybeSession =
    case maybeSession of
        Just session ->
            Sync.fetchOps session GotRemoteOps

        Nothing ->
            Cmd.none


subscriptions : Maybe Session -> Sub Msg
subscriptions session =
    Sub.batch
        [ Db.opsLoaded LocalOpsLoaded
        , case session of
            Just _ ->
                Time.every syncIntervalMs SyncTick

            Nothing ->
                Sub.none
        ]
