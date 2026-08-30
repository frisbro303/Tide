module Sea.FSRSTest exposing (suite)

import Expect
import Sea.FSRS as FSRS exposing (Rating(..))
import Test exposing (Test, describe, test)
import Time


addDays : Int -> Time.Posix -> Time.Posix
addDays days t =
    Time.millisToPosix (Time.posixToMillis t + days * 86400000)


{-| Reference case ported from fsrs-rs's `test_memory_state` (inference.rs),
which exercises the same FSRS-6 default weights this module hard-codes. Six
reviews — Again, then five Good — separated by 0, 0, 1, 3, 8, 21 elapsed
days should land on stability ≈ 53.62691 and difficulty ≈ 6.3574867.

<https://github.com/open-spaced-repetition/fsrs-rs/blob/main/src/inference.rs>
-}
suite : Test
suite =
    describe "FSRS.review against the fsrs-rs reference implementation"
        [ test "matches fsrs-rs's test_memory_state case" <|
            \_ ->
                let
                    t0 =
                        Time.millisToPosix 1700000000000

                    t1 =
                        t0

                    t2 =
                        addDays 1 t1

                    t3 =
                        addDays 3 t2

                    t4 =
                        addDays 8 t3

                    t5 =
                        addDays 21 t4

                    steps =
                        [ ( Again, t0 ), ( Good, t1 ), ( Good, t2 ), ( Good, t3 ), ( Good, t4 ), ( Good, t5 ) ]

                    finalState =
                        List.foldl
                            (\( rating, now ) state -> FSRS.review FSRS.defaultDesiredRetention now rating state)
                            (FSRS.initialState t0)
                            steps
                in
                Expect.all
                    [ \s -> s.stability |> Expect.within (Expect.Absolute 0.001) 53.62691
                    , \s -> s.difficulty |> Expect.within (Expect.Absolute 0.001) 6.3574867
                    ]
                    finalState
        ]
