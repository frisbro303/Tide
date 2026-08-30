module Sea.LearningTest exposing (suite)

import Expect
import Sea.Card as Card
import Sea.FSRS as FSRS exposing (Rating(..))
import Test exposing (Test, describe, test)
import Time
import UUID


cardId : Card.CardId
cardId =
    UUID.forName "learning-test" UUID.dnsNamespace


newCard : Card.Card
newCard =
    Card.new cardId "front" "back" (Time.millisToPosix 0)


suite : Test
suite =
    describe "Sea.Card learning steps"
        [ test "a fresh card starts on the first learning step" <|
            \_ ->
                Expect.equal (Just 0) newCard.learningStep
        , test "Again keeps a learning card new and resets it to the first step" <|
            \_ ->
                let
                    reviewed =
                        Card.review FSRS.defaultDesiredRetention (Time.millisToPosix 0) Again newCard
                in
                Expect.equal ( Just 0, True ) ( reviewed.learningStep, FSRS.isNew reviewed.fsrs )
        , test "Good on the last step graduates the card into FSRS scheduling" <|
            \_ ->
                let
                    afterFirstGood =
                        Card.review FSRS.defaultDesiredRetention (Time.millisToPosix 0) Good newCard

                    afterSecondGood =
                        Card.review FSRS.defaultDesiredRetention (Time.millisToPosix 0) Good afterFirstGood
                in
                Expect.equal ( Nothing, False ) ( afterSecondGood.learningStep, FSRS.isNew afterSecondGood.fsrs )
        , test "Easy graduates a card immediately, skipping the remaining steps" <|
            \_ ->
                let
                    reviewed =
                        Card.review FSRS.defaultDesiredRetention (Time.millisToPosix 0) Easy newCard
                in
                Expect.equal ( Nothing, False ) ( reviewed.learningStep, FSRS.isNew reviewed.fsrs )
        , test "a still-learning card is due within the same session, not a day+ out" <|
            \_ ->
                let
                    reviewed =
                        Card.review FSRS.defaultDesiredRetention (Time.millisToPosix 0) Again newCard

                    dueInMs =
                        Time.posixToMillis reviewed.fsrs.due
                in
                Expect.lessThan (60 * 60 * 1000) dueInMs
        ]
