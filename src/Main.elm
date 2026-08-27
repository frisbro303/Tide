module Main exposing (main)

import Add
import Browser
import Browser.Events
import Html exposing (Html, a, button, div, p, span, text)
import Html.Attributes exposing (attribute, class, classList, href, rel, target)
import List.Extra
import Html.Events exposing (onClick)
import Json.Decode as Decode
import Local.Store as Store
import Page exposing (Page)
import Review
import Sea.FSRS exposing (Rating(..))
import Sea.Sea as Sea
import Settings
import Stats
import Sync.Account as Account
import Sync.LocalOps as LocalOps
import Sync.Session as Session exposing (Session)
import Sync.SessionLifecycle as SessionLifecycle


type alias Model =
    { account : Account.Model
    , session : Maybe Session
    , localOps : LocalOps.Model
    , add : Add.Model
    , review : Review.Model
    , stats : Stats.Model
    , settings : Settings.Model
    , page : Page
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
                  , add = Add.init (Settings.typstPreamble Settings.default)
                  , review = Review.init
                  , stats = statsModel
                  , settings = Settings.default
                  , page = Page.Review
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
                    ( model, Cmd.map AccountMsg (Account.refresh session.refreshToken) )

                Nothing ->
                    case Settings.decodeFromStore loadedKey value of
                        Just ( settings, settingsCmd ) ->
                            ( { model | settings = settings }, Cmd.map SettingsMsg settingsCmd )

                        Nothing ->
                            ( model, Cmd.none )

        LocalOpsMsg localOpsMsg ->
            let
                ( localOpsModel, cmd ) =
                    LocalOps.update model.session localOpsMsg model.localOps
            in
            ( { model | localOps = localOpsModel }
            , Cmd.batch [ Cmd.map LocalOpsMsg cmd, pickIfIdle model.review ]
            )

        AddMsg addMsg ->
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
            in
            ( { afterOutModel | add = addModel }
            , Cmd.batch [ Cmd.map AddMsg addCmd, outCmd ]
            )

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
                ( settingsModel, settingsCmd ) =
                    Settings.update settingsMsg model.settings
            in
            ( { model | settings = settingsModel }, Cmd.map SettingsMsg settingsCmd )

        PageToggled page ->
            togglePage page model

        KeyPressed event ->
            handleKeyPressed event model


handleReviewMsg : Review.Msg -> Model -> ( Model, Cmd Msg )
handleReviewMsg reviewMsg model =
    let
        sea =
            Sea.fromOpsLog (Settings.desiredRetention model.settings) model.localOps

        ( reviewModel, reviewCmd, outMsg ) =
            Review.update (Settings.typstPreamble model.settings) sea reviewMsg model.review

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
                Add.init (Settings.typstPreamble model.settings)

            else
                model.add

        pageCmd =
            case newPage of
                Page.Review ->
                    pickIfIdle model.review

                Page.Stats ->
                    Cmd.map StatsMsg Stats.requestSummary

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
-- once revealed) edits the back, only once revealed.


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

    else
        ( model, Cmd.none )


handleMetaKey : String -> Model -> ( Model, Cmd Msg )
handleMetaKey key model =
    case String.toInt key |> Maybe.andThen (\n -> List.Extra.getAt (n - 1) Page.toggleable) of
        Just page ->
            togglePage page model

        Nothing ->
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
        [ pageBar model.page
        , div [ class "page-content" ] [ pageContent model ]
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


pageBar : Page -> Html Msg
pageBar currentPage =
    div [ class "page-bar", attribute "data-tauri-drag-region" "true" ]
        (List.map (pageIcon currentPage) Page.toggleable)


pageIcon : Page -> Page -> Html Msg
pageIcon currentPage page =
    button
        [ class "page-icon"
        , classList [ ( "page-icon-active", currentPage == page ) ]
        , onClick (PageToggled page)
        ]
        [ Page.icon page
        , span [ class "tooltip" ] [ text (pageTooltip page) ]
        ]


pageTooltip : Page -> String
pageTooltip page =
    case Page.shortcutLabel page of
        Just shortcut ->
            Page.label page ++ " (" ++ shortcut ++ ")"

        Nothing ->
            Page.label page


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
        , div [ class "settings-support" ]
            [ a
                [ class "settings-support-link"
                , href "https://www.buymeacoffee.com/frisbro"
                , target "_blank"
                , rel "noopener noreferrer"
                ]
                [ span [ class "bmc-icon" ] []
                , text "Buy Me a Coffee"
                ]
            ]
        ]


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Store.loaded StoreLoaded
        , Sub.map LocalOpsMsg (LocalOps.subscriptions model.session)
        , Sub.map AddMsg (Add.subscriptions model.add)
        , Sub.map ReviewMsg (Review.subscriptions model.review)
        , Sub.map SettingsMsg (Settings.subscriptions model.settings)
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
