module Sea.FSRS exposing
    ( Rating(..)
    , State
    , dateOf
    , defaultDesiredRetention
    , elapsedDays
    , initialState
    , isNew
    , retrievability
    , review
    )

import Array exposing (Array)
import Date exposing (Date)
import Time exposing (Posix)


type Rating
    = Again
    | Hard
    | Good
    | Easy


type alias State =
    { due : Posix
    , stability : Float
    , difficulty : Float
    , lastReview : Posix
    }


{-| The hour (UTC) at which a new day begins, for the purposes of counting
elapsed days between reviews (FSRS operates on whole calendar days, not raw
durations) — matches Anki's default rollover hour.
-}
rolloverHour : Int
rolloverHour =
    4


dateOf : Posix -> Date
dateOf t =
    Date.fromPosix Time.utc (Time.millisToPosix (Time.posixToMillis t - rolloverHour * 3600000))


elapsedDays : Posix -> Posix -> Int
elapsedDays from to =
    Date.diff Date.Days (dateOf from) (dateOf to)


{-| FSRS-6 default parameters, w0..w20. See <https://github.com/open-spaced-repetition/awesome-fsrs/wiki/The-Algorithm>.
-}
w : Array Float
w =
    Array.fromList
        [ 0.212, 1.2931, 2.3065, 8.2956, 6.4133, 0.8334, 3.0194, 0.001, 1.8722, 0.1666
        , 0.796, 1.4835, 0.0614, 0.2629, 1.6483, 0.6014, 1.8729, 0.5425, 0.0912, 0.0658
        , 0.1542
        ]


p i =
    Array.get i w |> Maybe.withDefault 0


defaultDesiredRetention : Float
defaultDesiredRetention =
    0.9


decay =
    -(p 20)


factor =
    0.9 ^ (1 / decay) - 1


ratingNumber rating =
    case rating of
        Again -> 1
        Hard -> 2
        Good -> 3
        Easy -> 4


retrievability elapsed stability =
    (1 + factor * max 0 elapsed / stability) ^ decay


initialStability rating =
    p (round (ratingNumber rating) - 1)


initialDifficulty rating =
    clamp 1 10 (p 4 - e ^ (p 5 * (ratingNumber rating - 1)) + 1)


nextDifficulty difficulty rating =
    let
        damped =
            difficulty - p 6 * (ratingNumber rating - 3) * (10 - difficulty) / 9
    in
    clamp 1 10 (p 7 * initialDifficulty Easy + (1 - p 7) * damped)


shortTermStability stability rating =
    stability * e ^ (p 17 * (ratingNumber rating - 3 + p 18)) * stability ^ -(p 19)


recallStability difficulty stability r rating =
    let
        bonus =
            if rating == Hard then
                p 15

            else if rating == Easy then
                p 16

            else
                1
    in
    stability * (e ^ p 8 * (11 - difficulty) * stability ^ -(p 9) * (e ^ (p 10 * (1 - r)) - 1) * bonus + 1)


forgetStability difficulty stability r =
    min stability (p 11 * difficulty ^ -(p 12) * ((stability + 1) ^ p 13 - 1) * e ^ (p 14 * (1 - r)))


nextIntervalDays desiredRetention stability =
    max 1 ((stability / factor) * (desiredRetention ^ (1 / decay) - 1))


addDays days t =
    Time.millisToPosix (Time.posixToMillis t + round (days * 86400000))


initialState : Posix -> State
initialState now =
    { due = now, stability = 0, difficulty = 0, lastReview = now }


{-| A `difficulty` of 0 marks a card that has never been reviewed, since a
reviewed card's difficulty is always clamped to the range [1, 10].
-}
isNew state =
    state.difficulty == 0


review : Float -> Posix -> Rating -> State -> State
review desiredRetention now rating state =
    let
        ( newDifficulty, newStability ) =
            if isNew state then
                ( initialDifficulty rating, initialStability rating )

            else
                let
                    elapsed =
                        toFloat (elapsedDays state.lastReview now)

                    r =
                        retrievability elapsed state.stability

                    stability =
                        if elapsed <= 0 then
                            shortTermStability state.stability rating

                        else if rating == Again then
                            forgetStability state.difficulty state.stability r

                        else
                            recallStability state.difficulty state.stability r rating
                in
                ( nextDifficulty state.difficulty rating, clamp 0.01 36500 stability )
    in
    { due = addDays (nextIntervalDays desiredRetention newStability) now
    , stability = newStability
    , difficulty = newDifficulty
    , lastReview = now
    }
