module Typst.EditableTypst exposing (Model, Msg, currentSource, init, initWithSource, isBlank, requestFocus, subscriptions, update, view)

import Browser.Events
import Html exposing (Html, div, img, span, text, textarea)
import Html.Attributes exposing (attribute, class, classList, id, placeholder, spellcheck, src, style, value)
import Html.Events exposing (on, onBlur, onClick, onInput, preventDefaultOn)
import Json.Decode as Decode
import Typst.Highlight as Highlight
import Typst.Port as Port
import Url



-- Shows the last committed render by default; clicking (or having nothing
-- committed yet) switches to a textarea with a live preview of the draft
-- below it. Blurring the textarea commits the draft.


type alias Model =
    { id : String
    , fieldPlaceholder : String
    , shortcutHint : String
    , preamble : String
    , committedSource : String
    , committedResult : Result String String
    , draftSource : String
    , draftResult : Result String String
    , manualEditing : Bool
    , fieldHeight : Float
    , drag : Maybe Drag
    , scrollTop : Float
    , highlightTree : Maybe Highlight.Node
    }


type alias Drag =
    { startY : Float
    , startHeight : Float
    }


defaultFieldHeight : Float
defaultFieldHeight =
    96


minFieldHeight : Float
minFieldHeight =
    64


maxFieldHeight : Float
maxFieldHeight =
    480


init : String -> String -> String -> String -> Model
init id fieldPlaceholder shortcutHint preamble =
    { id = id
    , fieldPlaceholder = fieldPlaceholder
    , shortcutHint = shortcutHint
    , preamble = preamble
    , committedSource = ""
    , committedResult = Err ""
    , draftSource = ""
    , draftResult = Err ""
    , manualEditing = False
    , fieldHeight = defaultFieldHeight
    , drag = Nothing
    , scrollTop = 0
    , highlightTree = Nothing
    }



-- For an already-existing card (e.g. loaded for review): opens showing the
-- committed source's compiled preview rather than an empty edit box.


initWithSource : String -> String -> String -> String -> String -> ( Model, Cmd Msg )
initWithSource id fieldPlaceholder shortcutHint preamble existingSource =
    ( { id = id
      , fieldPlaceholder = fieldPlaceholder
      , shortcutHint = shortcutHint
      , preamble = preamble
      , committedSource = existingSource
      , committedResult = Err ""
      , draftSource = existingSource
      , draftResult = Err ""
      , manualEditing = False
      , fieldHeight = defaultFieldHeight
      , drag = Nothing
      , scrollTop = 0
      , highlightTree = Nothing
      }
    , if String.trim existingSource == "" then
        Cmd.none

      else
        Cmd.batch
            [ Port.compileTypst id preamble existingSource
            , Port.highlightTypst id existingSource
            ]
    )


isEditing : Model -> Bool
isEditing model =
    model.manualEditing || isBlank model


isBlank : Model -> Bool
isBlank model =
    String.trim (currentSource model) == ""



-- Whatever the user has actually typed, even if it hasn't been committed
-- (blurred) yet — submitting shouldn't depend on blur having already fired.


currentSource : Model -> String
currentSource model =
    if model.manualEditing then
        model.draftSource

    else
        model.committedSource


type Msg
    = EditStarted
    | FocusRequested
    | DraftChanged String
    | GotDraftResult String (Result String String)
    | GotHighlightTree String Decode.Value
    | Committed
    | HandlePressed Float
    | HandleDragged Float
    | HandleReleased
    | Scrolled Float


requestFocus : Msg
requestFocus =
    FocusRequested


{-| The third element is `Just newSource` exactly when `Committed` just
changed the committed source (mirrors the reference's `if (value === text)
return;` no-op-if-unchanged) — callers that auto-save on commit (e.g. editing
a card mid-review) should react to it; everything else can ignore it.
-}
update : Msg -> Model -> ( Model, Cmd Msg, Maybe String )
update msg model =
    case msg of
        EditStarted ->
            if isEditing model then
                -- Already editing (e.g. clicking inside the textarea to
                -- place the cursor) — don't clobber the in-progress draft.
                ( model, Cmd.none, Nothing )

            else
                ( { model
                    | manualEditing = True
                    , draftSource = model.committedSource
                    , draftResult = model.committedResult
                  }
                , Port.focusField (textareaId model)
                , Nothing
                )

        FocusRequested ->
            ( if model.manualEditing then
                model

              else
                { model
                    | manualEditing = True
                    , draftSource = model.committedSource
                    , draftResult = model.committedResult
                }
            , Port.focusField (textareaId model)
            , Nothing
            )

        DraftChanged newSource ->
            ( { model | draftSource = newSource }
            , Cmd.batch
                [ Port.compileTypst model.id model.preamble newSource
                , Port.highlightTypst model.id newSource
                ]
            , Nothing
            )

        GotDraftResult requestId result ->
            if requestId /= model.id then
                ( model, Cmd.none, Nothing )

            else if isEditing model then
                -- Covers both actively typing (manualEditing) and a fresh
                -- blank field (isBlank implies isEditing even though
                -- manualEditing is still False) — either way the textarea
                -- is showing, so the result belongs in the draft preview.
                ( { model | draftResult = result }, Cmd.none, Nothing )

            else
                ( { model | committedResult = result }, Cmd.none, Nothing )

        GotHighlightTree requestId value ->
            if requestId /= model.id then
                ( model, Cmd.none, Nothing )

            else
                ( { model | highlightTree = Decode.decodeValue Highlight.decoder value |> Result.toMaybe }
                , Cmd.none
                , Nothing
                )

        Committed ->
            let
                changed =
                    model.draftSource /= model.committedSource
            in
            ( { model
                | manualEditing = False
                , committedSource = model.draftSource
                , committedResult = model.draftResult
              }
            , Cmd.none
            , if changed then
                Just model.draftSource

              else
                Nothing
            )

        HandlePressed clientY ->
            ( { model | drag = Just { startY = clientY, startHeight = model.fieldHeight } }
            , Cmd.none
            , Nothing
            )

        HandleDragged clientY ->
            case model.drag of
                Just drag ->
                    ( { model | fieldHeight = clamp minFieldHeight maxFieldHeight (drag.startHeight + (clientY - drag.startY)) }
                    , Cmd.none
                    , Nothing
                    )

                Nothing ->
                    ( model, Cmd.none, Nothing )

        HandleReleased ->
            ( { model | drag = Nothing }, Cmd.none, Nothing )

        Scrolled scrollTop ->
            ( { model | scrollTop = scrollTop }, Cmd.none, Nothing )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Port.typstCompiled GotDraftResult
        , Port.typstHighlighted GotHighlightTree
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
    div
        [ class "review-box"
        , classList [ ( "review-box--editing", isEditing model ) ]
        , onClick EditStarted
        ]
        (if isEditing model then
            [ div [ class "editable-typst-edit" ]
                (div
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
                                    text model.draftSource
                            ]
                        ]
                    , textarea
                        [ id (textareaId model)
                        , class "note-editor-field"
                        , placeholder model.fieldPlaceholder
                        , value model.draftSource
                        , attribute "autocorrect" "off"
                        , attribute "autocapitalize" "off"
                        , spellcheck False
                        , onInput DraftChanged
                        , onBlur Committed
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
                    :: (if String.trim model.draftSource == "" then
                            []

                        else
                            [ div [ class "note-editor-preview" ] [ previewView model.draftResult ] ]
                       )
                )
            ]

         else
            [ previewView model.committedResult
            , span [ class "editable-typst-hint" ] [ text model.shortcutHint ]
            ]
        )


previewView : Result String String -> Html msg
previewView result =
    case result of
        Ok svg ->
            img [ src (svgDataUrl svg) ] []

        Err error ->
            div [ class "note-editor-error" ] [ text error ]


svgDataUrl : String -> String
svgDataUrl svg =
    "data:image/svg+xml;charset=utf-8," ++ Url.percentEncode svg


textareaId : Model -> String
textareaId model =
    "editable-typst-" ++ model.id
