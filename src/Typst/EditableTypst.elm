module Typst.EditableTypst exposing (Model, Msg, OutMsg(..), currentSource, init, initWithSource, isBlank, recompile, requestFocus, subscriptions, update, view)

import Browser.Events
import Dict exposing (Dict)
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
    , knownImages : Dict String String
    , pendingImages : Dict String String
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


init : String -> String -> String -> String -> Dict String String -> Model
init id fieldPlaceholder shortcutHint preamble knownImages =
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
    , knownImages = knownImages
    , pendingImages = Dict.empty
    }



-- For an already-existing card (e.g. loaded for review): opens showing the
-- committed source's compiled preview rather than an empty edit box.


initWithSource : String -> String -> String -> String -> Dict String String -> String -> ( Model, Cmd Msg )
initWithSource id fieldPlaceholder shortcutHint preamble knownImages existingSource =
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
      , knownImages = knownImages
      , pendingImages = Dict.empty
      }
    , if String.trim existingSource == "" then
        Cmd.none

      else
        Cmd.batch
            [ Port.compileTypst id preamble existingSource (imageAttachments knownImages Dict.empty existingSource)
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


referencedImageIds : String -> List String
referencedImageIds source =
    String.split "#image(\"" source
        |> List.drop 1
        |> List.filterMap
            (\chunk ->
                case String.split "\"" chunk of
                    first :: _ ->
                        if String.endsWith ".png" first then
                            Just (String.dropRight 4 first)

                        else
                            Nothing

                    [] ->
                        Nothing
            )


imageAttachments : Dict String String -> Dict String String -> String -> List ( String, String )
imageAttachments knownImages pendingImages source =
    let
        allImages =
            Dict.union pendingImages knownImages
    in
    referencedImageIds source
        |> List.filterMap (\imgId -> Dict.get imgId allImages |> Maybe.map (\data -> ( imgId ++ ".png", data )))


type Msg
    = EditStarted
    | FocusRequested
    | DraftChanged String
    | ImageReceived String String
    | GotDraftResult String (Result String String)
    | GotHighlightTree String Decode.Value
    | Committed
    | HandlePressed Float
    | HandleDragged Float
    | HandleReleased
    | Scrolled Float
    | Recompile


requestFocus : Msg
requestFocus =
    FocusRequested


recompile : Msg
recompile =
    Recompile


type OutMsg
    = NoOutMsg
    | SourceCommitted String
    | ImageAdded String String


update : Msg -> Model -> ( Model, Cmd Msg, OutMsg )
update msg model =
    case msg of
        EditStarted ->
            if isEditing model then
                -- Already editing (e.g. clicking inside the textarea to
                -- place the cursor) — don't clobber the in-progress draft.
                ( model, Cmd.none, NoOutMsg )

            else
                ( { model
                    | manualEditing = True
                    , draftSource = model.committedSource
                    , draftResult = model.committedResult
                  }
                , Port.focusField (textareaId model)
                , NoOutMsg
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
            , NoOutMsg
            )

        DraftChanged newSource ->
            ( { model | draftSource = newSource }
            , Cmd.batch
                [ Port.compileTypst model.id model.preamble newSource (imageAttachments model.knownImages model.pendingImages newSource)
                , Port.highlightTypst model.id newSource
                ]
            , NoOutMsg
            )

        ImageReceived imgId data ->
            let
                newModel =
                    { model | pendingImages = Dict.insert imgId data model.pendingImages }
            in
            ( newModel
            , Port.compileTypst newModel.id newModel.preamble newModel.draftSource (imageAttachments newModel.knownImages newModel.pendingImages newModel.draftSource)
            , ImageAdded imgId data
            )

        GotDraftResult requestId result ->
            if requestId /= model.id then
                ( model, Cmd.none, NoOutMsg )

            else if isEditing model then
                -- Covers both actively typing (manualEditing) and a fresh
                -- blank field (isBlank implies isEditing even though
                -- manualEditing is still False) — either way the textarea
                -- is showing, so the result belongs in the draft preview.
                ( { model | draftResult = result }, Cmd.none, NoOutMsg )

            else
                ( { model | committedResult = result }, Cmd.none, NoOutMsg )

        GotHighlightTree requestId value ->
            if requestId /= model.id then
                ( model, Cmd.none, NoOutMsg )

            else
                ( { model | highlightTree = Decode.decodeValue Highlight.decoder value |> Result.toMaybe }
                , Cmd.none
                , NoOutMsg
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
                SourceCommitted model.draftSource

              else
                NoOutMsg
            )

        HandlePressed clientY ->
            ( { model | drag = Just { startY = clientY, startHeight = model.fieldHeight } }
            , Cmd.none
            , NoOutMsg
            )

        HandleDragged clientY ->
            case model.drag of
                Just drag ->
                    ( { model | fieldHeight = clamp minFieldHeight maxFieldHeight (drag.startHeight + (clientY - drag.startY)) }
                    , Cmd.none
                    , NoOutMsg
                    )

                Nothing ->
                    ( model, Cmd.none, NoOutMsg )

        HandleReleased ->
            ( { model | drag = Nothing }, Cmd.none, NoOutMsg )

        Scrolled scrollTop ->
            ( { model | scrollTop = scrollTop }, Cmd.none, NoOutMsg )

        Recompile ->
            let
                source =
                    currentSource model
            in
            if String.trim source == "" then
                ( model, Cmd.none, NoOutMsg )

            else
                ( model
                , Port.compileTypst model.id model.preamble source (imageAttachments model.knownImages model.pendingImages source)
                , NoOutMsg
                )


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


imageEventDecoder : Decode.Decoder Msg
imageEventDecoder =
    Decode.map2 ImageReceived
        (Decode.at [ "detail", "id" ] Decode.string)
        (Decode.at [ "detail", "data" ] Decode.string)


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
                        , on "tide-image-added" imageEventDecoder
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
