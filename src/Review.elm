module Review exposing (Model, Msg, OutMsg(..), editBack, editFront, init, isIdle, isRevealed, rate, requestPick, reveal, subscriptions, themeChanged, update, view, viewActions)

import Dict exposing (Dict)
import Html exposing (Html, button, div, hr, p, span, text)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import Ops.Op exposing (Op, OpId(..), OpKind(..))
import Ops.OpsLog exposing (OpsLog)
import Random
import Sea.Card as Card
import Sea.FSRS exposing (Rating(..))
import Sea.Sea as Sea exposing (Sea)
import Task
import Time
import Typst.EditableTypst as EditableTypst
import UUID



-- Picks the next due/new card once (on page-entry) and again after every
-- rate/delete — not reactively on every Sea change, so an in-progress review
-- isn't yanked out from under the user by an unrelated background sync.


type Model
    = NotAsked
    | Empty
    | Reviewing CurrentCard


type alias CurrentCard =
    { id : Card.CardId
    , front : EditableTypst.Model
    , back : EditableTypst.Model
    , revealed : Bool
    , menuOpen : Bool
    }


init : Model
init =
    NotAsked



-- True unless a card is currently being reviewed — used by callers to avoid
-- re-picking (and discarding in-progress edits) when returning to a review
-- that's already underway.


isIdle : Model -> Bool
isIdle model =
    case model of
        Reviewing _ ->
            False

        _ ->
            True



-- True only while a card is showing and its answer has been revealed —
-- used by callers wiring up keyboard shortcuts to gate the rating keys.


isRevealed : Model -> Bool
isRevealed model =
    case model of
        Reviewing current ->
            current.revealed

        _ ->
            False


reveal : Msg
reveal =
    RevealClicked


editFront : Msg
editFront =
    FrontMsg EditableTypst.requestFocus


editBack : Msg
editBack =
    BackMsg EditableTypst.requestFocus


themeChanged : Msg
themeChanged =
    ThemeChanged


rate : Rating -> Msg
rate =
    RateClicked


requestPick : Cmd Msg
requestPick =
    Task.perform GotTimeForPick Time.now


type Msg
    = GotTimeForPick Time.Posix
    | FrontMsg EditableTypst.Msg
    | BackMsg EditableTypst.Msg
    | GotTimeForEdit Card.CardId Bool String Time.Posix
    | RevealClicked
    | RateClicked Rating
    | GotTimeForRate Card.CardId Rating Time.Posix
    | MenuToggled
    | DeleteClicked
    | GotTimeForDelete Card.CardId Time.Posix
    | AddCardClicked
    | GotTimeForImageOp String String Time.Posix
    | ThemeChanged


type OutMsg
    = NoOutMsg
    | Submitted Op
    | AddRequested
    | ImagePersisted Op


newOpId : Time.Posix -> OpId
newOpId now =
    Random.step UUID.generator (Random.initialSeed (Time.posixToMillis now))
        |> Tuple.first
        |> OpId


update : String -> Int -> Dict String String -> OpsLog -> Sea -> Msg -> Model -> ( Model, Cmd Msg, OutMsg )
update preamble dailyNewLimit knownImages opsLog sea msg model =
    case msg of
        GotTimeForPick now ->
            let
                allowNew =
                    Sea.newCardsToday now opsLog < dailyNewLimit
            in
            case Sea.nextDue allowNew now sea of
                Nothing ->
                    ( Empty, Cmd.none, NoOutMsg )

                Just card ->
                    let
                        ( frontModel, frontCmd ) =
                            EditableTypst.initWithSource "review-front" "Front" "i" preamble knownImages card.front

                        ( backModel, backCmd ) =
                            EditableTypst.initWithSource "review-back" "Back" "o" preamble knownImages card.back
                    in
                    ( Reviewing
                        { id = card.id
                        , front = frontModel
                        , back = backModel
                        , revealed = False
                        , menuOpen = False
                        }
                    , Cmd.batch [ Cmd.map FrontMsg frontCmd, Cmd.map BackMsg backCmd ]
                    , NoOutMsg
                    )

        FrontMsg frontMsg ->
            case model of
                Reviewing current ->
                    let
                        ( frontModel, cmd, outMsg ) =
                            EditableTypst.update frontMsg current.front

                        editCmd =
                            case outMsg of
                                EditableTypst.SourceCommitted newSource ->
                                    Task.perform (GotTimeForEdit current.id True newSource) Time.now

                                EditableTypst.ImageAdded imgId data ->
                                    Task.perform (GotTimeForImageOp imgId data) Time.now

                                EditableTypst.NoOutMsg ->
                                    Cmd.none
                    in
                    ( Reviewing { current | front = frontModel }
                    , Cmd.batch [ Cmd.map FrontMsg cmd, editCmd ]
                    , NoOutMsg
                    )

                _ ->
                    ( model, Cmd.none, NoOutMsg )

        BackMsg backMsg ->
            case model of
                Reviewing current ->
                    let
                        ( backModel, cmd, outMsg ) =
                            EditableTypst.update backMsg current.back

                        editCmd =
                            case outMsg of
                                EditableTypst.SourceCommitted newSource ->
                                    Task.perform (GotTimeForEdit current.id False newSource) Time.now

                                EditableTypst.ImageAdded imgId data ->
                                    Task.perform (GotTimeForImageOp imgId data) Time.now

                                EditableTypst.NoOutMsg ->
                                    Cmd.none
                    in
                    ( Reviewing { current | back = backModel }
                    , Cmd.batch [ Cmd.map BackMsg cmd, editCmd ]
                    , NoOutMsg
                    )

                _ ->
                    ( model, Cmd.none, NoOutMsg )

        GotTimeForEdit cardId isFront newSource now ->
            case model of
                Reviewing current ->
                    let
                        front =
                            if isFront then
                                newSource

                            else
                                EditableTypst.currentSource current.front

                        back =
                            if isFront then
                                EditableTypst.currentSource current.back

                            else
                                newSource

                        op =
                            { id = newOpId now
                            , timeStamp = now
                            , opKind = EditCard { id = cardId, front = front, back = back }
                            }
                    in
                    ( model, Cmd.none, Submitted op )

                _ ->
                    ( model, Cmd.none, NoOutMsg )

        RevealClicked ->
            case model of
                Reviewing current ->
                    ( Reviewing { current | revealed = True }, Cmd.none, NoOutMsg )

                _ ->
                    ( model, Cmd.none, NoOutMsg )

        RateClicked rating ->
            case model of
                Reviewing current ->
                    ( model, Task.perform (GotTimeForRate current.id rating) Time.now, NoOutMsg )

                _ ->
                    ( model, Cmd.none, NoOutMsg )

        GotTimeForRate cardId rating now ->
            let
                op =
                    { id = newOpId now
                    , timeStamp = now
                    , opKind = ReviewCard { id = cardId, rating = rating }
                    }
            in
            ( NotAsked, requestPick, Submitted op )

        MenuToggled ->
            case model of
                Reviewing current ->
                    ( Reviewing { current | menuOpen = not current.menuOpen }, Cmd.none, NoOutMsg )

                _ ->
                    ( model, Cmd.none, NoOutMsg )

        DeleteClicked ->
            case model of
                Reviewing current ->
                    ( model, Task.perform (GotTimeForDelete current.id) Time.now, NoOutMsg )

                _ ->
                    ( model, Cmd.none, NoOutMsg )

        GotTimeForDelete cardId now ->
            let
                op =
                    { id = newOpId now
                    , timeStamp = now
                    , opKind = DeleteCard cardId
                    }
            in
            ( NotAsked, requestPick, Submitted op )

        AddCardClicked ->
            ( model, Cmd.none, AddRequested )

        GotTimeForImageOp imgId data now ->
            let
                op =
                    { id = newOpId now
                    , timeStamp = now
                    , opKind = AddImage { id = imgId, data = data }
                    }
            in
            ( model, Cmd.none, ImagePersisted op )

        ThemeChanged ->
            case model of
                Reviewing current ->
                    let
                        ( frontModel, frontCmd, _ ) =
                            EditableTypst.update EditableTypst.recompile current.front

                        ( backModel, backCmd, _ ) =
                            EditableTypst.update EditableTypst.recompile current.back
                    in
                    ( Reviewing { current | front = frontModel, back = backModel }
                    , Cmd.batch [ Cmd.map FrontMsg frontCmd, Cmd.map BackMsg backCmd ]
                    , NoOutMsg
                    )

                _ ->
                    ( model, Cmd.none, NoOutMsg )


subscriptions : Model -> Sub Msg
subscriptions model =
    case model of
        Reviewing current ->
            Sub.batch
                [ Sub.map FrontMsg (EditableTypst.subscriptions current.front)
                , Sub.map BackMsg (EditableTypst.subscriptions current.back)
                ]

        _ ->
            Sub.none


view : Model -> Html Msg
view model =
    case model of
        NotAsked ->
            p [ class "review-empty" ] [ text "Loading..." ]

        Empty ->
            div [ class "review-empty-state" ]
                [ p [ class "review-empty" ] [ text "No cards due." ]
                , p [ class "review-empty review-empty-subtitle" ] [ text "Great work, you're all caught up." ]
                , button [ class "button-primary", onClick AddCardClicked ] [ text "Add a card now" ]
                ]

        Reviewing current ->
            div [ class "review-stage" ]
                [ div [ class "card-menu" ]
                    (button [ class "page-icon", onClick MenuToggled ]
                        [ text "\u{22EF}" ]
                        :: (if current.menuOpen then
                                [ div [ class "context-menu-overlay", onClick MenuToggled ] []
                                , div [ class "card-menu-popover" ]
                                    [ button
                                        [ class "context-menu-item context-menu-item--danger"
                                        , onClick DeleteClicked
                                        ]
                                        [ text "Delete" ]
                                    ]
                                ]

                            else
                                []
                           )
                    )
                , div [ class "review-card-area" ]
                    (Html.map FrontMsg (EditableTypst.view current.front)
                        :: (if current.revealed then
                                [ hr [ class "review-divider" ] []
                                , Html.map BackMsg (EditableTypst.view current.back)
                                ]

                            else
                                []
                           )
                    )
                ]



-- Rendered separately from `view` so callers can place it outside their
-- scrollable content container — nesting `position: fixed` inside an
-- `overflow: auto` ancestor is unreliable in WKWebView.


viewActions : Model -> Html Msg
viewActions model =
    case model of
        Reviewing current ->
            div [ class "review-fixed-content" ]
                [ div [ class "review-actions" ]
                    (if current.revealed then
                        [ div [ class "grade-buttons" ]
                            [ ratingButton "review-rating--again" (RateClicked Again) "Again" "1"
                            , ratingButton "review-rating--hard" (RateClicked Hard) "Hard" "2"
                            , ratingButton "review-rating--good" (RateClicked Good) "Good" "3"
                            , ratingButton "review-rating--easy" (RateClicked Easy) "Easy" "4"
                            ]
                        ]

                     else
                        [ button [ class "review-reveal", onClick RevealClicked ]
                            [ text "Show answer", span [ class "btn-hint" ] [ text "Space" ] ]
                        ]
                    )
                ]

        _ ->
            text ""


ratingButton : String -> Msg -> String -> String -> Html Msg
ratingButton modifierClass msg label hint =
    button [ class ("review-rating " ++ modifierClass), onClick msg ]
        [ text label, span [ class "btn-hint" ] [ text hint ] ]


