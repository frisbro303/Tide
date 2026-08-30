module Sea.Learning exposing (Outcome(..), advance, initialStep)

import Sea.FSRS exposing (Rating(..))
import Time exposing (Posix)


{-| Minutes between same-session re-shows for a card still in its learning
steps. Fixed and short (not user-configurable), matching Anki's default new
card steps ("1m 10m") — this exists precisely because FSRS-6's stability
model is fit on day-scale review gaps and has no opinion on minute-scale
scheduling. A card leaves this queue (and starts being scheduled by FSRS)
once it passes the last step on a Good rating, or immediately on Easy.
-}
steps : List Float
steps =
    [ 1, 10 ]


initialStep : Int
initialStep =
    0


type Outcome
    = StillLearning { step : Int, due : Posix }
    | Graduated


advance : Posix -> Rating -> Int -> Outcome
advance now rating step =
    case rating of
        Easy ->
            Graduated

        Again ->
            StillLearning { step = initialStep, due = addMinutes (stepMinutes initialStep) now }

        Hard ->
            -- Simplified relative to Anki (which averages the current and
            -- next step): just repeats the current step's delay.
            StillLearning { step = step, due = addMinutes (stepMinutes step) now }

        Good ->
            let
                nextStep =
                    step + 1
            in
            case stepMinutesAt nextStep of
                Just minutes ->
                    StillLearning { step = nextStep, due = addMinutes minutes now }

                Nothing ->
                    Graduated


stepMinutes : Int -> Float
stepMinutes step =
    stepMinutesAt step |> Maybe.withDefault 1


stepMinutesAt : Int -> Maybe Float
stepMinutesAt step =
    steps |> List.drop step |> List.head


addMinutes : Float -> Posix -> Posix
addMinutes minutes t =
    Time.millisToPosix (Time.posixToMillis t + round (minutes * 60000))
