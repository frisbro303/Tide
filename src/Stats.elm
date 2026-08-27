module Stats exposing (History, HistoryDay, Model, Msg, Summary, init, requestSummary, summarize, update, view)

import Date exposing (Date)
import Dict exposing (Dict)
import Html exposing (Html, div, h3, option, p, select, span, text)
import Html.Attributes exposing (class, selected, value)
import Html.Events exposing (onInput)
import Ops.Op exposing (OpKind(..))
import Ops.OpsLog as OpsLog exposing (OpsLog)
import Sea.FSRS as FSRS exposing (Rating(..))
import Sea.Sea as Sea exposing (Sea)
import Set
import Svg
import Svg.Attributes as SA
import Task
import Time



-- "Learning" here is a pure approximation (this codebase, like the FSRS
-- algorithm itself, has no distinct Anki-style learning-steps state) — a
-- reviewed card counts as "learning" whenever its current scheduled
-- interval is under a day. Not mutually exclusive with "due": a card can be
-- both overdue and still short-interval.


type alias Summary =
    { total : Int
    , due : Int
    , learning : Int
    , new : Int
    , dailyStreak : Int
    , retainedPercent : Maybe Int
    }


summarize : Time.Posix -> OpsLog -> Sea -> Summary
summarize now opsLog sea =
    let
        cards =
            Sea.toList sea

        ( newCards, reviewedCards ) =
            List.partition (\card -> FSRS.isNew card.fsrs) cards

        dueCards =
            List.filter (\card -> Time.posixToMillis card.fsrs.due <= Time.posixToMillis now) reviewedCards

        learningCards =
            List.filter (\card -> FSRS.elapsedDays card.fsrs.lastReview card.fsrs.due < 1) reviewedCards

        retainedPercent =
            case reviewedCards of
                [] ->
                    Nothing

                _ ->
                    let
                        retrievabilityOf card =
                            FSRS.retrievability
                                (toFloat (max 0 (FSRS.elapsedDays card.fsrs.lastReview now)))
                                card.fsrs.stability
                    in
                    Just
                        (round
                            (100
                                * (List.map retrievabilityOf reviewedCards |> List.sum)
                                / toFloat (List.length reviewedCards)
                            )
                        )
    in
    { total = List.length cards
    , due = List.length dueCards
    , learning = List.length learningCards
    , new = List.length newCards
    , dailyStreak = dailyStreak now opsLog
    , retainedPercent = retainedPercent
    }



-- Consecutive calendar days (by the same day-rollover rule the scheduler
-- itself uses) with at least one review, counting backward from today — or
-- from yesterday if today has no review yet, so the streak doesn't zero out
-- until a full day passes with none.


dailyStreak : Time.Posix -> OpsLog -> Int
dailyStreak now opsLog =
    let
        reviewedDays =
            OpsLog.toList opsLog
                |> List.filterMap
                    (\op ->
                        case op.opKind of
                            ReviewCard _ ->
                                Just (dayNumber op.timeStamp)

                            _ ->
                                Nothing
                    )
                |> Set.fromList

        today =
            dayNumber now

        startDay =
            if Set.member today reviewedDays then
                today

            else
                today - 1

        countBack day =
            if Set.member day reviewedDays then
                1 + countBack (day - 1)

            else
                0
    in
    countBack startDay


dayNumber : Time.Posix -> Int
dayNumber t =
    Date.toRataDie (FSRS.dateOf t)



-- Per-day rating counts for the last `days` days (oldest first), for the
-- review-history chart and the retention breakdown below it.


type alias DayCounts =
    { again : Int, hard : Int, good : Int, easy : Int }


emptyDayCounts : DayCounts
emptyDayCounts =
    { again = 0, hard = 0, good = 0, easy = 0 }


addRating : Rating -> DayCounts -> DayCounts
addRating rating counts =
    case rating of
        Again ->
            { counts | again = counts.again + 1 }

        Hard ->
            { counts | hard = counts.hard + 1 }

        Good ->
            { counts | good = counts.good + 1 }

        Easy ->
            { counts | easy = counts.easy + 1 }


type alias HistoryDay =
    { date : Date, counts : DayCounts }


type alias History =
    List HistoryDay


history : Time.Posix -> Int -> OpsLog -> History
history now days opsLog =
    let
        clampedDays =
            clamp 1 730 days

        today =
            FSRS.dateOf now

        byDay : Dict Int DayCounts
        byDay =
            OpsLog.toList opsLog
                |> List.filterMap
                    (\op ->
                        case op.opKind of
                            ReviewCard { rating } ->
                                Just ( Date.toRataDie (FSRS.dateOf op.timeStamp), rating )

                            _ ->
                                Nothing
                    )
                |> List.foldl
                    (\( day, rating ) acc ->
                        Dict.update day
                            (Maybe.withDefault emptyDayCounts >> addRating rating >> Just)
                            acc
                    )
                    Dict.empty
    in
    List.range 0 (clampedDays - 1)
        |> List.map
            (\offset ->
                let
                    day =
                        Date.add Date.Days (offset - (clampedDays - 1)) today
                in
                { date = day
                , counts = Dict.get (Date.toRataDie day) byDay |> Maybe.withDefault emptyDayCounts
                }
            )


type alias RatingTotals =
    { again : Int, hard : Int, good : Int, easy : Int, total : Int }


retentionTotals : History -> RatingTotals
retentionTotals hist =
    let
        sumBy getter =
            List.sum (List.map (getter << .counts) hist)

        again =
            sumBy .again

        hard =
            sumBy .hard

        good =
            sumBy .good

        easy =
            sumBy .easy
    in
    { again = again, hard = hard, good = good, easy = easy, total = again + hard + good + easy }


type Model
    = Loading
    | Loaded LoadedState


type alias LoadedState =
    { summary : Summary
    , historyDays : Int
    , history : History
    }


defaultHistoryDays : Int
defaultHistoryDays =
    30


init : ( Model, Cmd Msg )
init =
    ( Loading, requestSummary )


requestSummary : Cmd Msg
requestSummary =
    Task.perform GotTimeForSummary Time.now


type Msg
    = GotTimeForSummary Time.Posix
    | HistoryDaysChanged Int
    | GotTimeForHistory Int Time.Posix


update : OpsLog -> Sea -> Msg -> Model -> ( Model, Cmd Msg )
update opsLog sea msg model =
    case msg of
        GotTimeForSummary now ->
            ( Loaded
                { summary = summarize now opsLog sea
                , historyDays = defaultHistoryDays
                , history = history now defaultHistoryDays opsLog
                }
            , Cmd.none
            )

        HistoryDaysChanged days ->
            ( model, Task.perform (GotTimeForHistory days) Time.now )

        GotTimeForHistory days now ->
            case model of
                Loaded state ->
                    ( Loaded { state | historyDays = days, history = history now days opsLog }, Cmd.none )

                Loading ->
                    ( model, Cmd.none )


view : Model -> Html Msg
view model =
    case model of
        Loading ->
            p [ class "stats-loading" ] [ text "Loading..." ]

        Loaded state ->
            let
                totals =
                    retentionTotals state.history
            in
            div []
                [ h3 [ class "history-heading" ] [ text "General" ]
                , div [ class "stats-grid" ]
                    [ statTile (String.fromInt state.summary.total) "Total cards"
                    , statTile (String.fromInt state.summary.due) "Due now"
                    , statTile (String.fromInt state.summary.learning) "Learning"
                    , statTile (String.fromInt state.summary.new) "New"
                    , statTile (dailyStreakLabel state.summary.dailyStreak) "Daily streak"
                    , statTile (retainedLabel state.summary.retainedPercent) "Retained now"
                    ]
                , div [ class "history-heading-row" ]
                    [ h3 [ class "history-heading" ] [ text "Reviews" ]
                    , viewDaysSelect state.historyDays
                    ]
                , div [ class "history-card" ]
                    [ viewHistoryChart state.history
                    , viewHistoryLegend
                    ]
                , h3 [ class "history-heading retention-heading" ] [ text "Retention" ]
                , div [ class "history-card" ]
                    [ div [ class "retention-row" ]
                        [ div [ class "retention-chart-wrap" ] [ viewRetentionDonut totals ]
                        , viewRetentionLegend totals
                        ]
                    ]
                ]


dailyStreakLabel : Int -> String
dailyStreakLabel streak =
    String.fromInt streak
        ++ (if streak == 1 then
                " day"

            else
                " days"
           )


retainedLabel : Maybe Int -> String
retainedLabel maybePercent =
    case maybePercent of
        Just percent ->
            String.fromInt percent ++ "%"

        Nothing ->
            "—"


statTile : String -> String -> Html msg
statTile val label =
    div [ class "stat-tile" ]
        [ div [ class "stat-value" ] [ text val ]
        , div [ class "stat-label" ] [ text label ]
        ]


viewDaysSelect : Int -> Html Msg
viewDaysSelect selectedDays =
    select
        [ class "link-select"
        , onInput (String.toInt >> Maybe.withDefault selectedDays >> HistoryDaysChanged)
        ]
        (List.map (daysOption selectedDays) [ 7, 30, 90, 365 ])


daysOption : Int -> Int -> Html Msg
daysOption selectedDays days =
    option [ value (String.fromInt days), selected (days == selectedDays) ]
        [ text (daysLabel days) ]


daysLabel : Int -> String
daysLabel days =
    case days of
        7 ->
            "Last 7 days"

        30 ->
            "Last 30 days"

        90 ->
            "Last 90 days"

        365 ->
            "Last year"

        _ ->
            String.fromInt days ++ " days"



-- Charts. Tooltips use plain SVG <title> (native browser hover tooltip)
-- rather than a custom cursor-following bubble — much less code, no
-- pointer-position tracking needed, at the cost of the reference's fancier
-- styled tooltip look.


ratingSegments : List ( DayCounts -> Int, String, String )
ratingSegments =
    [ ( .easy, "hist-bar-easy", "Easy" )
    , ( .good, "hist-bar-good", "Good" )
    , ( .hard, "hist-bar-hard", "Hard" )
    , ( .again, "hist-bar-again", "Again" )
    ]


viewHistoryChart : History -> Html msg
viewHistoryChart hist =
    let
        chartWidth =
            600

        chartHeight =
            140

        plotHeight =
            chartHeight - 16

        n =
            List.length hist

        slot =
            chartWidth / toFloat (max 1 n)

        barGap =
            min 2 (slot * 0.2)

        barWidth =
            max 0.5 (slot - barGap)

        maxTotal =
            hist
                |> List.map (\d -> toFloat (d.counts.again + d.counts.hard + d.counts.good + d.counts.easy))
                |> List.maximum
                |> Maybe.withDefault 0
                |> max 1

        labelEvery =
            max 1 (ceiling (toFloat n / 6))

        barsFor i day =
            let
                x =
                    toFloat i * (barWidth + barGap)

                place ( getter, cls, label ) ( yTop, acc ) =
                    let
                        count =
                            getter day.counts
                    in
                    if count == 0 then
                        ( yTop, acc )

                    else
                        let
                            segHeight =
                                (toFloat count / maxTotal) * plotHeight

                            newY =
                                yTop - segHeight

                            rect =
                                Svg.rect
                                    [ SA.class cls
                                    , SA.x (numAttr x)
                                    , SA.y (numAttr newY)
                                    , SA.width (numAttr barWidth)
                                    , SA.height (numAttr segHeight)
                                    ]
                                    [ Svg.title []
                                        [ text (formatMonthDay day.date ++ ": " ++ String.fromInt count ++ " " ++ String.toLower label) ]
                                    ]
                        in
                        ( newY, rect :: acc )
            in
            Tuple.second (List.foldl place ( plotHeight, [] ) ratingSegments)

        bars =
            List.indexedMap barsFor hist |> List.concat

        labels =
            hist
                |> List.indexedMap Tuple.pair
                |> List.filter (\( i, _ ) -> modBy labelEvery i == 0)
                |> List.map
                    (\( i, day ) ->
                        Svg.text_
                            [ SA.class "hist-axis-label"
                            , SA.x (numAttr (toFloat i * (barWidth + barGap) + barWidth / 2))
                            , SA.y (numAttr (chartHeight - 3))
                            , SA.textAnchor "middle"
                            ]
                            [ text (formatMonthDay day.date) ]
                    )
    in
    Svg.svg
        [ SA.class "history-chart"
        , SA.viewBox ("0 0 " ++ numAttr chartWidth ++ " " ++ numAttr chartHeight)
        ]
        (bars ++ labels)


viewHistoryLegend : Html msg
viewHistoryLegend =
    div [ class "history-legend" ]
        (List.map
            (\( _, cls, label ) ->
                span [ class "history-legend-item" ]
                    [ span [ class ("history-swatch " ++ cls) ] []
                    , span [] [ text label ]
                    ]
            )
            ratingSegments
        )


viewRetentionDonut : RatingTotals -> Html msg
viewRetentionDonut totals =
    let
        size =
            120

        r =
            50

        c =
            size / 2
    in
    if totals.total == 0 then
        Svg.svg [ SA.class "retention-donut", SA.viewBox (numAttr 0 ++ " 0 " ++ numAttr size ++ " " ++ numAttr size) ]
            [ Svg.circle
                [ SA.class "retention-empty", SA.cx (numAttr c), SA.cy (numAttr c), SA.r (numAttr r) ]
                [ Svg.title [] [ text "No reviews yet" ] ]
            ]

    else
        let
            wedgeFor ( label, count, cls ) ( angleSoFar, acc ) =
                if count == 0 then
                    ( angleSoFar, acc )

                else
                    let
                        nextAngle =
                            angleSoFar + (toFloat count / toFloat totals.total) * 360

                        percent =
                            round (toFloat count / toFloat totals.total * 100)

                        wedge =
                            Svg.path
                                [ SA.class cls, SA.d (pieWedgePath c c r angleSoFar nextAngle) ]
                                [ Svg.title [] [ text (label ++ ": " ++ String.fromInt count ++ " (" ++ String.fromInt percent ++ "%)") ] ]
                    in
                    ( nextAngle, wedge :: acc )

            wedges =
                List.foldl wedgeFor
                    ( 0, [] )
                    [ ( "Again", totals.again, "hist-bar-again" )
                    , ( "Hard", totals.hard, "hist-bar-hard" )
                    , ( "Good", totals.good, "hist-bar-good" )
                    , ( "Easy", totals.easy, "hist-bar-easy" )
                    ]
                    |> Tuple.second
                    |> List.reverse
        in
        Svg.svg [ SA.class "retention-donut", SA.viewBox (numAttr 0 ++ " 0 " ++ numAttr size ++ " " ++ numAttr size) ] wedges


viewRetentionLegend : RatingTotals -> Html msg
viewRetentionLegend totals =
    let
        entries =
            [ ( "Again", totals.again, "hist-bar-again" )
            , ( "Hard", totals.hard, "hist-bar-hard" )
            , ( "Good", totals.good, "hist-bar-good" )
            , ( "Easy", totals.easy, "hist-bar-easy" )
            ]
    in
    div [ class "retention-legend" ]
        (List.map
            (\( label, count, cls ) ->
                span [ class "history-legend-item" ]
                    [ span [ class ("history-swatch " ++ cls) ] []
                    , span []
                        [ text
                            (label
                                ++ (if totals.total > 0 then
                                        ": " ++ String.fromInt (round (toFloat count / toFloat totals.total * 100)) ++ "%"

                                    else
                                        ""
                                   )
                            )
                        ]
                    ]
            )
            entries
        )


polarPoint : Float -> Float -> Float -> Float -> ( Float, Float )
polarPoint cx cy r angleDeg =
    let
        rad =
            (angleDeg - 90) * pi / 180
    in
    ( cx + r * cos rad, cy + r * sin rad )


pieWedgePath : Float -> Float -> Float -> Float -> Float -> String
pieWedgePath cx cy r startAngle endAngle =
    if endAngle - startAngle >= 360 then
        "M " ++ numAttr cx ++ " " ++ numAttr (cy - r) ++ " A " ++ numAttr r ++ " " ++ numAttr r ++ " 0 1 1 " ++ numAttr (cx - 0.01) ++ " " ++ numAttr (cy - r) ++ " Z"

    else
        let
            ( sx, sy ) =
                polarPoint cx cy r startAngle

            ( ex, ey ) =
                polarPoint cx cy r endAngle

            largeArc =
                if endAngle - startAngle > 180 then
                    "1"

                else
                    "0"
        in
        "M " ++ numAttr cx ++ " " ++ numAttr cy ++ " L " ++ numAttr sx ++ " " ++ numAttr sy ++ " A " ++ numAttr r ++ " " ++ numAttr r ++ " 0 " ++ largeArc ++ " 1 " ++ numAttr ex ++ " " ++ numAttr ey ++ " Z"


formatMonthDay : Date -> String
formatMonthDay date =
    padInt (Date.monthNumber date) ++ "/" ++ padInt (Date.day date)


padInt : Int -> String
padInt n =
    String.padLeft 2 '0' (String.fromInt n)


numAttr : Float -> String
numAttr =
    String.fromFloat
