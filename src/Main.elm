module Main exposing (main)

import Add
import Browser
import Browser.Events
import Data
import Dict exposing (Dict)
import Html exposing (Html, a, button, div, h3, span, text)
import Html.Attributes exposing (attribute, class, classList, href, rel, target)
import List.Extra
import Html.Events exposing (onClick)
import Json.Decode as Decode
import Json.Encode as Encode
import Local.Store as Store
import LucideIcons
import Ops.Op as Op
import Ops.OpsLog as OpsLog exposing (OpsLog)
import Page exposing (Page)
import Random
import Review
import Sea.FSRS exposing (Rating(..))
import Sea.Sea as Sea
import Settings
import Stats
import Svg.Attributes exposing (height, width)
import Sync.Account as Account
import Sync.LocalOps as LocalOps
import Sync.Session as Session exposing (Session)
import Sync.SessionLifecycle as SessionLifecycle
import Task
import Time
import UUID


type alias Model =
    { account : Account.Model
    , session : Maybe Session
    , localOps : LocalOps.Model
    , add : Add.Model
    , review : Review.Model
    , stats : Stats.Model
    , settings : Settings.Model
    , page : Page
    , syncError : Maybe String
    }


type Msg
    = AccountMsg Account.Msg
    | StoreLoaded String Decode.Value
    | LocalOpsMsg LocalOps.Msg
    | AddMsg Add.Msg
    | ReviewMsg Review.Msg
    | StatsMsg Stats.Msg
    | SettingsMsg Settings.Msg
    | PageToggled Page
    | KeyPressed KeyEvent
    | ExportDataClicked
    | ImportDataClicked
    | GotImportedJson String
    | GotTimeForPreambleOp String Time.Posix
    | GotTimeForRetentionOp Int Time.Posix


type alias KeyEvent =
    { key : String
    , meta : Bool
    , isTextInput : Bool
    }


main : Program () Model Msg
main =
    Browser.element
        { init =
            \_ ->
                let
                    ( localOpsModel, localOpsCmd ) =
                        LocalOps.init

                    ( statsModel, statsCmd ) =
                        Stats.init
                in
                ( { account = Account.init
                  , session = Nothing
                  , localOps = localOpsModel
                  , add = Add.init (Settings.typstPreamble Settings.default) (latestImages localOpsModel)
                  , review = Review.init
                  , stats = statsModel
                  , settings = Settings.default
                  , page = Page.Review
                  , syncError = Nothing
                  }
                , Cmd.batch
                    [ Session.request
                    , Settings.request
                    , Cmd.map LocalOpsMsg localOpsCmd
                    , Cmd.map ReviewMsg Review.requestPick
                    , Cmd.map StatsMsg statsCmd
                    ]
                )
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        ( newModel, cmd ) =
            updateInner msg model

        ( preambleModel, preambleCmd ) =
            syncPreambleFromOps newModel

        ( syncedModel, retentionCmd ) =
            syncRetentionFromOps preambleModel
    in
    ( syncedModel, Cmd.batch [ cmd, preambleCmd, retentionCmd ] )


latestOpValue : (Op.OpKind -> Maybe a) -> OpsLog -> Maybe a
latestOpValue extract opsLog =
    OpsLog.foldl
        (\op acc ->
            case extract op.opKind of
                Just value ->
                    case acc of
                        Just ( t, _ ) ->
                            if Time.posixToMillis op.timeStamp > Time.posixToMillis t then
                                Just ( op.timeStamp, value )

                            else
                                acc

                        Nothing ->
                            Just ( op.timeStamp, value )

                Nothing ->
                    acc
        )
        Nothing
        opsLog
        |> Maybe.map Tuple.second


latestPreamble : OpsLog -> Maybe String
latestPreamble =
    latestOpValue
        (\opKind ->
            case opKind of
                Op.SetPreamble preamble ->
                    Just preamble

                _ ->
                    Nothing
        )


latestRetention : OpsLog -> Maybe Int
latestRetention =
    latestOpValue
        (\opKind ->
            case opKind of
                Op.SetRetention retentionPercent ->
                    Just retentionPercent

                _ ->
                    Nothing
        )


latestImages : OpsLog -> Dict String String
latestImages opsLog =
    OpsLog.foldl
        (\op acc ->
            case op.opKind of
                Op.AddImage { id, data } ->
                    Dict.insert id data acc

                _ ->
                    acc
        )
        Dict.empty
        opsLog


syncPreambleFromOps : Model -> ( Model, Cmd Msg )
syncPreambleFromOps model =
    case latestPreamble model.localOps of
        Just preamble ->
            if preamble == Settings.typstPreamble model.settings then
                ( model, Cmd.none )

            else
                let
                    ( settingsModel, settingsCmd ) =
                        Settings.applySyncedPreamble preamble model.settings
                in
                ( { model | settings = settingsModel }, Cmd.map SettingsMsg settingsCmd )

        Nothing ->
            ( model, Cmd.none )


syncRetentionFromOps : Model -> ( Model, Cmd Msg )
syncRetentionFromOps model =
    case latestRetention model.localOps of
        Just retentionPercent ->
            if retentionPercent == model.settings.retentionPercent then
                ( model, Cmd.none )

            else
                let
                    ( settingsModel, settingsCmd ) =
                        Settings.applySyncedRetention retentionPercent model.settings
                in
                ( { model | settings = settingsModel }, Cmd.map SettingsMsg settingsCmd )

        Nothing ->
            ( model, Cmd.none )


applySyncStatus : LocalOps.SyncStatus -> Model -> Model
applySyncStatus status model =
    case status of
        LocalOps.NoStatusChange ->
            model

        LocalOps.SyncFailed message ->
            { model | syncError = Just message }

        LocalOps.SyncSucceeded ->
            { model | syncError = Nothing }


newOpId : Time.Posix -> Op.OpId
newOpId now =
    Random.step UUID.generator (Random.initialSeed (Time.posixToMillis now))
        |> Tuple.first
        |> Op.OpId


updateInner : Msg -> Model -> ( Model, Cmd Msg )
updateInner msg model =
    case msg of
        AccountMsg accountMsg ->
            let
                ( accountModel, cmd, sessionUpdate ) =
                    Account.update accountMsg model.account

                ( updatedModel, localOpsCmd ) =
                    SessionLifecycle.apply sessionUpdate { model | account = accountModel }
            in
            ( updatedModel
            , Cmd.batch [ Cmd.map AccountMsg cmd, Cmd.map LocalOpsMsg localOpsCmd ]
            )

        StoreLoaded loadedKey value ->
            case Session.decodeFromStore loadedKey value of
                Just session ->
                    let
                        ( updatedModel, localOpsCmd ) =
                            SessionLifecycle.apply (Account.SessionEstablished session) model
                    in
                    ( updatedModel
                    , Cmd.batch
                        [ Cmd.map LocalOpsMsg localOpsCmd
                        , Cmd.map AccountMsg (Account.refresh session.refreshToken)
                        ]
                    )

                Nothing ->
                    case Settings.decodeFromStore loadedKey value of
                        Just ( settings, settingsCmd ) ->
                            ( { model | settings = settings }, Cmd.map SettingsMsg settingsCmd )

                        Nothing ->
                            ( model, Cmd.none )

        LocalOpsMsg localOpsMsg ->
            let
                ( localOpsModel, cmd, syncStatus ) =
                    LocalOps.update model.session localOpsMsg model.localOps
            in
            ( applySyncStatus syncStatus { model | localOps = localOpsModel }
            , Cmd.batch [ Cmd.map LocalOpsMsg cmd, pickIfIdle model.review ]
            )

        AddMsg addMsg ->
            handleAddMsg addMsg model



        ReviewMsg reviewMsg ->
            handleReviewMsg reviewMsg model

        StatsMsg statsMsg ->
            let
                sea =
                    Sea.fromOpsLog (Settings.desiredRetention model.settings) model.localOps

                ( statsModel, statsCmd ) =
                    Stats.update model.localOps sea statsMsg model.stats
            in
            ( { model | stats = statsModel }, Cmd.map StatsMsg statsCmd )

        SettingsMsg settingsMsg ->
            let
                ( settingsModel, settingsCmd, syncUpdate ) =
                    Settings.update settingsMsg model.settings

                themeChanged =
                    settingsModel.theme /= model.settings.theme

                opCmd =
                    case syncUpdate of
                        Settings.PreambleCommitted preamble ->
                            if Just preamble == latestPreamble model.localOps then
                                Cmd.none

                            else
                                Task.perform (GotTimeForPreambleOp preamble) Time.now

                        Settings.RetentionCommitted retentionPercent ->
                            if Just retentionPercent == latestRetention model.localOps then
                                Cmd.none

                            else
                                Task.perform (GotTimeForRetentionOp retentionPercent) Time.now

                        Settings.NoSyncUpdate ->
                            Cmd.none

                modelAfterSettings =
                    { model | settings = settingsModel }

                ( modelAfterTheme, themeCmd ) =
                    if themeChanged then
                        let
                            ( modelAfterAdd, addCmd ) =
                                handleAddMsg Add.themeChanged modelAfterSettings

                            ( modelAfterReview, reviewCmd ) =
                                handleReviewMsg Review.themeChanged modelAfterAdd
                        in
                        ( modelAfterReview, Cmd.batch [ addCmd, reviewCmd ] )

                    else
                        ( modelAfterSettings, Cmd.none )
            in
            ( modelAfterTheme
            , Cmd.batch [ Cmd.map SettingsMsg settingsCmd, opCmd, themeCmd ]
            )

        GotTimeForPreambleOp preamble now ->
            let
                op =
                    { id = newOpId now, timeStamp = now, opKind = Op.SetPreamble preamble }

                ( localOpsModel, cmd ) =
                    LocalOps.insertNewOp op model.localOps
            in
            ( { model | localOps = localOpsModel }, Cmd.map LocalOpsMsg cmd )

        GotTimeForRetentionOp retentionPercent now ->
            let
                op =
                    { id = newOpId now, timeStamp = now, opKind = Op.SetRetention retentionPercent }

                ( localOpsModel, cmd ) =
                    LocalOps.insertNewOp op model.localOps
            in
            ( { model | localOps = localOpsModel }, Cmd.map LocalOpsMsg cmd )

        PageToggled page ->
            togglePage page model

        KeyPressed event ->
            handleKeyPressed event model

        ExportDataClicked ->
            ( model, Data.exportData (Encode.list Op.encoder (OpsLog.toList model.localOps)) )

        ImportDataClicked ->
            ( model, Data.requestImport )

        GotImportedJson raw ->
            case Decode.decodeString (Decode.list Op.decoder) raw of
                Ok ops ->
                    let
                        ( localOpsModel, cmd, syncStatus ) =
                            LocalOps.update model.session (LocalOps.ImportedOps (OpsLog.fromList ops)) model.localOps
                    in
                    ( applySyncStatus syncStatus { model | localOps = localOpsModel }, Cmd.map LocalOpsMsg cmd )

                Err _ ->
                    ( model, Cmd.none )


handleAddMsg : Add.Msg -> Model -> ( Model, Cmd Msg )
handleAddMsg addMsg model =
    let
        ( addModel, addCmd, outMsg ) =
            Add.update addMsg model.add

        ( afterOutModel, outCmd ) =
            case outMsg of
                Add.NoOutMsg ->
                    ( model, Cmd.none )

                Add.Submitted op ->
                    let
                        ( localOpsModel, localOpsCmd ) =
                            LocalOps.insertNewOp op model.localOps
                    in
                    ( { model | localOps = localOpsModel, page = Page.Review }
                    , Cmd.batch [ Cmd.map LocalOpsMsg localOpsCmd, pickIfIdle model.review ]
                    )

                Add.Canceled ->
                    ( { model | page = Page.Review }, pickIfIdle model.review )

                Add.ImagePersisted op ->
                    let
                        ( localOpsModel, localOpsCmd ) =
                            LocalOps.insertNewOp op model.localOps
                    in
                    ( { model | localOps = localOpsModel }, Cmd.map LocalOpsMsg localOpsCmd )
    in
    ( { afterOutModel | add = addModel }
    , Cmd.batch [ Cmd.map AddMsg addCmd, outCmd ]
    )


handleReviewMsg : Review.Msg -> Model -> ( Model, Cmd Msg )
handleReviewMsg reviewMsg model =
    let
        sea =
            Sea.fromOpsLog (Settings.desiredRetention model.settings) model.localOps

        ( reviewModel, reviewCmd, outMsg ) =
            Review.update (Settings.typstPreamble model.settings) (Settings.dailyNewLimit model.settings) (latestImages model.localOps) model.localOps sea reviewMsg model.review

        ( afterOutModel, outCmd ) =
            case outMsg of
                Review.NoOutMsg ->
                    ( model, Cmd.none )

                Review.Submitted op ->
                    let
                        ( localOpsModel, localOpsCmd ) =
                            LocalOps.insertNewOp op model.localOps
                    in
                    ( { model | localOps = localOpsModel }, Cmd.map LocalOpsMsg localOpsCmd )

                Review.AddRequested ->
                    ( { model | page = Page.Add }, Cmd.none )

                Review.ImagePersisted op ->
                    let
                        ( localOpsModel, localOpsCmd ) =
                            LocalOps.insertNewOp op model.localOps
                    in
                    ( { model | localOps = localOpsModel }, Cmd.map LocalOpsMsg localOpsCmd )
    in
    ( { afterOutModel | review = reviewModel }
    , Cmd.batch [ Cmd.map ReviewMsg reviewCmd, outCmd ]
    )


togglePage : Page -> Model -> ( Model, Cmd Msg )
togglePage page model =
    let
        togglingOff =
            model.page == page

        newPage =
            if togglingOff then
                Page.Review

            else
                page

        -- Toggling Add off via its own icon/shortcut is how Add is
        -- cancelled, so it discards the draft — but switching away to a
        -- *different* page (Settings, Stats, Account) leaves it untouched,
        -- so it's still there if you come back to Add later.
        newAdd =
            if togglingOff && model.page == Page.Add then
                Add.init (Settings.typstPreamble model.settings) (latestImages model.localOps)

            else
                model.add

        pageCmd =
            case newPage of
                Page.Review ->
                    pickIfIdle model.review

                Page.Stats ->
                    Cmd.map StatsMsg Stats.requestSummary

                Page.Account ->
                    Cmd.map LocalOpsMsg (LocalOps.requestSync model.session)

                _ ->
                    Cmd.none
    in
    ( { model | page = newPage, add = newAdd }, pageCmd )


pickIfIdle : Review.Model -> Cmd Msg
pickIfIdle reviewModel =
    if Review.isIdle reviewModel then
        Cmd.map ReviewMsg Review.requestPick

    else
        Cmd.none



-- Cmd/Ctrl+1..4 switch pages (toggling the active one back to Review, same
-- as clicking its icon); Escape always returns to Review. On the Review page
-- itself (and only when focus isn't in a text field): Space/Enter reveals,
-- Space also rates Good once revealed, 1-4 rate Again/Hard/Good/Easy. Editing
-- front/back uses bare vim/helix-style normal-mode keys rather than a Cmd
-- combo — Cmd+W in particular is macOS's reserved "close window" shortcut,
-- and a global key subscription can't preventDefault it, so the OS wins and
-- closes the app before our handler ever sees the keystroke. "i" (insert)
-- edits the front; "o" (open-below, since back appears below the divider
-- once revealed) edits the back — on Review only once revealed; on Add both
-- are available immediately since neither field is gated behind a reveal.


handleKeyPressed : KeyEvent -> Model -> ( Model, Cmd Msg )
handleKeyPressed event model =
    if event.isTextInput then
        ( model, Cmd.none )

    else if event.meta then
        handleMetaKey event.key model

    else if event.key == "Escape" then
        togglePage Page.Review model

    else if model.page == Page.Review then
        handleReviewKey event.key model

    else if model.page == Page.Add then
        handleAddKey event.key model

    else
        ( model, Cmd.none )


handleMetaKey : String -> Model -> ( Model, Cmd Msg )
handleMetaKey key model =
    case String.toInt key |> Maybe.andThen (\n -> List.Extra.getAt (n - 1) Page.toggleable) of
        Just page ->
            togglePage page model

        Nothing ->
            ( model, Cmd.none )


handleAddKey : String -> Model -> ( Model, Cmd Msg )
handleAddKey key model =
    case key of
        "i" ->
            handleAddMsg Add.editFront model

        "o" ->
            handleAddMsg Add.editBack model

        _ ->
            ( model, Cmd.none )


handleReviewKey : String -> Model -> ( Model, Cmd Msg )
handleReviewKey key model =
    let
        revealed =
            Review.isRevealed model.review
    in
    case key of
        " " ->
            handleReviewMsg
                (if revealed then
                    Review.rate Good

                 else
                    Review.reveal
                )
                model

        "Enter" ->
            if revealed then
                ( model, Cmd.none )

            else
                handleReviewMsg Review.reveal model

        "1" ->
            rateIfRevealed revealed Again model

        "2" ->
            rateIfRevealed revealed Hard model

        "3" ->
            rateIfRevealed revealed Good model

        "4" ->
            rateIfRevealed revealed Easy model

        "i" ->
            handleReviewMsg Review.editFront model

        "o" ->
            if revealed then
                handleReviewMsg Review.editBack model

            else
                ( model, Cmd.none )

        _ ->
            ( model, Cmd.none )


rateIfRevealed : Bool -> Rating -> Model -> ( Model, Cmd Msg )
rateIfRevealed revealed rating model =
    if revealed then
        handleReviewMsg (Review.rate rating) model

    else
        ( model, Cmd.none )


view : Model -> Html Msg
view model =
    div [ class "app" ]
        [ pageBar model
        , div [ class "page-content" ] [ pageContent model ]
        , settingsSupportBar model
        , fixedBottomActions model
        ]


fixedBottomActions : Model -> Html Msg
fixedBottomActions model =
    case model.page of
        Page.Review ->
            div [ class "review-fixed-bottom" ]
                [ Html.map ReviewMsg (Review.viewActions model.review) ]

        _ ->
            text ""


pageBar : Model -> Html Msg
pageBar model =
    div [ class "page-bar", attribute "data-tauri-drag-region" "true" ]
        (List.map (pageIcon model) Page.toggleable)


type AccountStatus
    = StatusHidden
    | StatusOk
    | StatusError String


accountStatus : Model -> AccountStatus
accountStatus model =
    case model.session of
        Nothing ->
            StatusHidden

        Just _ ->
            case model.syncError of
                Nothing ->
                    StatusOk

                Just message ->
                    StatusError message


pageIcon : Model -> Page -> Html Msg
pageIcon model page =
    let
        status =
            if page == Page.Account then
                accountStatus model

            else
                StatusHidden
    in
    button
        [ class "page-icon"
        , classList [ ( "page-icon-active", model.page == page ) ]
        , onClick (PageToggled page)
        ]
        ([ Page.icon page
         , span [ class "tooltip" ] [ text (pageTooltip status page) ]
         ]
            ++ (case status of
                    StatusHidden ->
                        []

                    StatusOk ->
                        [ span [ class "page-icon-dot page-icon-dot--ok" ] [] ]

                    StatusError _ ->
                        [ span [ class "page-icon-dot page-icon-dot--error" ] [] ]
               )
        )


pageTooltip : AccountStatus -> Page -> String
pageTooltip status page =
    let
        label =
            case Page.shortcutLabel page of
                Just shortcut ->
                    Page.label page ++ " (" ++ shortcut ++ ")"

                Nothing ->
                    Page.label page
    in
    case status of
        StatusError message ->
            label ++ " — " ++ message

        _ ->
            label


pageContent : Model -> Html Msg
pageContent model =
    case model.page of
        Page.Review ->
            Html.map ReviewMsg (Review.view model.review)

        Page.Add ->
            Html.map AddMsg (Add.view model.add)

        Page.Stats ->
            div [ class "stats-card" ] [ Html.map StatsMsg (Stats.view model.stats) ]

        Page.Account ->
            Html.map AccountMsg (Account.view model.session model.account)

        Page.Settings ->
            settingsView model


settingsView : Model -> Html Msg
settingsView model =
    div [ class "settings-body" ]
        [ Html.map SettingsMsg (Settings.view model.settings)
        , div [ class "settings-section" ]
            [ h3 [ class "history-heading" ] [ text "Data" ]
            , div [ class "settings-fields" ]
                [ div [ class "settings-field" ]
                    [ div [ class "settings-actions" ]
                        [ button [ class "button-ghost", onClick ExportDataClicked ] [ text "Export data" ]
                        , button [ class "button-ghost", onClick ImportDataClicked ] [ text "Import data" ]
                        ]
                    ]
                ]
            ]
        ]


settingsSupportBar : Model -> Html Msg
settingsSupportBar model =
    case model.page of
        Page.Settings ->
            div [ class "settings-support-bar" ]
                [ a
                    [ class "settings-support-link"
                    , href "https://www.buymeacoffee.com/frisbro"
                    , target "_blank"
                    , rel "noopener noreferrer"
                    ]
                    [ LucideIcons.coffeeIcon [ width "16", height "16" ]
                    , text "Buy Me a Coffee"
                    ]
                ]

        _ ->
            text ""


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Store.loaded StoreLoaded
        , Sub.map LocalOpsMsg (LocalOps.subscriptions model.session)
        , Sub.map AddMsg (Add.subscriptions model.add)
        , Sub.map ReviewMsg (Review.subscriptions model.review)
        , Sub.map SettingsMsg (Settings.subscriptions model.settings)
        , Data.importLoaded GotImportedJson
        , Browser.Events.onKeyDown keyEventDecoder |> Sub.map KeyPressed
        ]


keyEventDecoder : Decode.Decoder KeyEvent
keyEventDecoder =
    Decode.map4
        (\key metaKey ctrlKey tagName ->
            { key = key
            , meta = metaKey || ctrlKey
            , isTextInput = tagName == "INPUT" || tagName == "TEXTAREA"
            }
        )
        (Decode.field "key" Decode.string)
        (Decode.field "metaKey" Decode.bool)
        (Decode.field "ctrlKey" Decode.bool)
        (Decode.at [ "target", "tagName" ] Decode.string)
