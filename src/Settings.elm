module Settings exposing (Model, Msg, PreambleUpdate(..), applySyncedPreamble, dailyNewLimit, decodeFromStore, default, desiredRetention, request, subscriptions, typstPreamble, update, view)

import Browser.Events
import Html exposing (Html, div, h3, label, option, select, text, textarea)
import Html.Attributes exposing (attribute, class, for, id, placeholder, selected, spellcheck, style, type_, value)
import Html.Attributes as Attr
import Html.Events exposing (on, onBlur, onInput, preventDefaultOn)
import Json.Decode as Decode
import Json.Encode as Encode
import Local.Store as Store
import Theme exposing (Theme)
import Typst.Highlight as Highlight
import Typst.Port as Port


type alias Model =
    { retentionPercent : Int
    , dailyNewLimit : Int
    , typstPreamble : String
    , theme : Theme
    , highlightTree : Maybe Highlight.Node
    , fieldHeight : Float
    , drag : Maybe Drag
    , scrollTop : Float
    }


type alias Drag =
    { startY : Float
    , startHeight : Float
    }


defaultFieldHeight : Float
defaultFieldHeight =
    128


minFieldHeight : Float
minFieldHeight =
    64


maxFieldHeight : Float
maxFieldHeight =
    480


default : Model
default =
    { retentionPercent = 90
    , dailyNewLimit = 20
    , typstPreamble = ""
    , theme = Theme.System
    , highlightTree = Nothing
    , fieldHeight = defaultFieldHeight
    , drag = Nothing
    , scrollTop = 0
    }


desiredRetention : Model -> Float
desiredRetention model =
    toFloat model.retentionPercent / 100


typstPreamble : Model -> String
typstPreamble model =
    model.typstPreamble


dailyNewLimit : Model -> Int
dailyNewLimit model =
    model.dailyNewLimit


key : String
key =
    "settings"


encoder : Model -> Encode.Value
encoder model =
    Encode.object
        [ ( "retentionPercent", Encode.int model.retentionPercent )
        , ( "dailyNewLimit", Encode.int model.dailyNewLimit )
        , ( "typstPreamble", Encode.string model.typstPreamble )
        , ( "theme", Encode.string (Theme.toString model.theme) )
        ]


type alias StoredFields =
    { retentionPercent : Maybe Int
    , dailyNewLimit : Maybe Int
    , typstPreamble : Maybe String
    , theme : Maybe String
    }


decoder : Decode.Decoder StoredFields
decoder =
    Decode.map4 StoredFields
        (Decode.maybe (Decode.field "retentionPercent" Decode.int))
        (Decode.maybe (Decode.field "dailyNewLimit" Decode.int))
        (Decode.maybe (Decode.field "typstPreamble" Decode.string))
        (Decode.maybe (Decode.field "theme" Decode.string))


decodeFromStore : String -> Decode.Value -> Maybe ( Model, Cmd Msg )
decodeFromStore loadedKey val =
    if loadedKey == key then
        Decode.decodeValue decoder val
            |> Result.toMaybe
            |> Maybe.map
                (\fields ->
                    let
                        model =
                            { default
                                | retentionPercent = Maybe.withDefault default.retentionPercent fields.retentionPercent
                                , dailyNewLimit = Maybe.withDefault default.dailyNewLimit fields.dailyNewLimit
                                , typstPreamble = Maybe.withDefault default.typstPreamble fields.typstPreamble
                                , theme = fields.theme |> Maybe.map Theme.fromString |> Maybe.withDefault default.theme
                            }
                    in
                    ( model
                    , Cmd.batch
                        [ Port.highlightTypst preambleFieldId model.typstPreamble
                        , Theme.setTheme model.theme
                        ]
                    )
                )

    else
        Nothing


request : Cmd msg
request =
    Store.get key


save : Model -> Cmd msg
save model =
    Store.set key (encoder model)


preambleFieldId : String
preambleFieldId =
    "settings-preamble"


type Msg
    = RetentionChanged String
    | DailyNewLimitChanged String
    | ThemeChanged String
    | PreambleChanged String
    | PreambleBlurred
    | GotHighlightTree String Decode.Value
    | HandlePressed Float
    | HandleDragged Float
    | HandleReleased
    | Scrolled Float


type PreambleUpdate
    = PreambleUnchanged
    | PreambleCommitted String


update : Msg -> Model -> ( Model, Cmd Msg, PreambleUpdate )
update msg model =
    case msg of
        RetentionChanged raw ->
            case String.toInt raw of
                Just percent ->
                    let
                        newModel =
                            { model | retentionPercent = clamp 50 99 percent }
                    in
                    ( newModel, save newModel, PreambleUnchanged )

                Nothing ->
                    ( model, Cmd.none, PreambleUnchanged )

        DailyNewLimitChanged raw ->
            case String.toInt raw of
                Just n ->
                    let
                        newModel =
                            { model | dailyNewLimit = clamp 0 500 n }
                    in
                    ( newModel, save newModel, PreambleUnchanged )

                Nothing ->
                    ( model, Cmd.none, PreambleUnchanged )

        ThemeChanged raw ->
            let
                newModel =
                    { model | theme = Theme.fromString raw }
            in
            ( newModel, Cmd.batch [ save newModel, Theme.setTheme newModel.theme ], PreambleUnchanged )

        PreambleChanged text_ ->
            ( { model | typstPreamble = text_ }, Port.highlightTypst preambleFieldId text_, PreambleUnchanged )

        PreambleBlurred ->
            ( model, save model, PreambleCommitted model.typstPreamble )

        GotHighlightTree requestId value ->
            if requestId /= preambleFieldId then
                ( model, Cmd.none, PreambleUnchanged )

            else
                ( { model | highlightTree = Decode.decodeValue Highlight.decoder value |> Result.toMaybe }
                , Cmd.none
                , PreambleUnchanged
                )

        HandlePressed clientY ->
            ( { model | drag = Just { startY = clientY, startHeight = model.fieldHeight } }, Cmd.none, PreambleUnchanged )

        HandleDragged clientY ->
            case model.drag of
                Just drag ->
                    ( { model | fieldHeight = clamp minFieldHeight maxFieldHeight (drag.startHeight + (clientY - drag.startY)) }
                    , Cmd.none
                    , PreambleUnchanged
                    )

                Nothing ->
                    ( model, Cmd.none, PreambleUnchanged )

        HandleReleased ->
            ( { model | drag = Nothing }, Cmd.none, PreambleUnchanged )

        Scrolled scrollTop ->
            ( { model | scrollTop = scrollTop }, Cmd.none, PreambleUnchanged )


applySyncedPreamble : String -> Model -> ( Model, Cmd Msg )
applySyncedPreamble preamble model =
    let
        newModel =
            { model | typstPreamble = preamble }
    in
    ( newModel, Cmd.batch [ save newModel, Port.highlightTypst preambleFieldId preamble ] )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Port.typstHighlighted GotHighlightTree
        , case model.drag of
            Just _ ->
                Sub.batch
                    [ Browser.Events.onMouseMove (Decode.map HandleDragged (Decode.field "clientY" Decode.float))
                    , Browser.Events.onMouseUp (Decode.succeed HandleReleased)
                    ]

            Nothing ->
                Sub.none
        ]


view : Model -> Html Msg
view model =
    div [ class "settings-sections" ]
        [ settingsSection "Review"
            [ div [ class "settings-field" ]
                [ label [ for "settings-retention" ] [ text "Desired retention (%)" ]
                , Html.input
                    [ id "settings-retention"
                    , class "auth-input"
                    , type_ "number"
                    , Attr.min "50"
                    , Attr.max "99"
                    , value (String.fromInt model.retentionPercent)
                    , onInput RetentionChanged
                    ]
                    []
                ]
            , div [ class "settings-field" ]
                [ label [ for "settings-daily-new-limit" ] [ text "New cards per day" ]
                , Html.input
                    [ id "settings-daily-new-limit"
                    , class "auth-input"
                    , type_ "number"
                    , Attr.min "0"
                    , Attr.max "500"
                    , value (String.fromInt model.dailyNewLimit)
                    , onInput DailyNewLimitChanged
                    ]
                    []
                ]
            ]
        , settingsSection "Appearance"
            [ div [ class "settings-field" ]
                [ label [ for "settings-theme" ] [ text "Theme" ]
                , select
                    [ id "settings-theme"
                    , class "auth-input"
                    , onInput ThemeChanged
                    ]
                    [ option [ value "system", selected (model.theme == Theme.System) ] [ text "System" ]
                    , option [ value "light", selected (model.theme == Theme.Light) ] [ text "Light" ]
                    , option [ value "dark", selected (model.theme == Theme.Dark) ] [ text "Dark" ]
                    ]
                ]
            ]
        , settingsSection "Typst"
            [ div [ class "settings-field" ]
                [ label [ for preambleFieldId ] [ text "Preamble" ]
                , div [ class "settings-preamble-wrap" ]
                    [ div
                        [ class "note-editor-field-wrap"
                        , style "height" (String.fromFloat model.fieldHeight ++ "px")
                        ]
                        [ div [ class "note-editor-highlight" ]
                            [ div
                                [ class "note-editor-highlight-scroll"
                                , style "transform" ("translateY(-" ++ String.fromFloat model.scrollTop ++ "px)")
                                ]
                                [ case model.highlightTree of
                                    Just tree ->
                                        Highlight.view tree

                                    Nothing ->
                                        text model.typstPreamble
                                ]
                            ]
                        , textarea
                            [ id preambleFieldId
                            , class "note-editor-field"
                            , placeholder "Typst preamble"
                            , value model.typstPreamble
                            , attribute "autocorrect" "off"
                            , attribute "autocapitalize" "off"
                            , spellcheck False
                            , onInput PreambleChanged
                            , onBlur PreambleBlurred
                            , on "scroll" (Decode.map Scrolled (Decode.at [ "target", "scrollTop" ] Decode.float))
                            ]
                            []
                        , div
                            [ class "note-editor-resize-handle"
                            , preventDefaultOn "mousedown"
                                (Decode.map (\clientY -> ( HandlePressed clientY, True )) (Decode.field "clientY" Decode.float))
                            ]
                            []
                        ]
                    ]
                ]
            ]
        ]


settingsSection : String -> List (Html Msg) -> Html Msg
settingsSection title fields =
    div [ class "settings-section" ]
        [ h3 [ class "history-heading" ] [ text title ]
        , div [ class "settings-fields" ] fields
        ]
