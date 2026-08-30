module Sea.Sea exposing (Sea, applyOp, emptySea, fromOpsLog, getCard, getDue, insertCard, newCardsToday, nextDue, removeCard, size, toList, updateCard)

import Dict exposing (Dict)
import Ops.Op exposing (Op, OpKind(..))
import Ops.OpsLog as OpsLog exposing (OpsLog)
import Sea.Card as Card
import Sea.FSRS as FSRS
import Time exposing (Posix)
import UUID


type Sea
    = Sea { cards : Dict String Card.Card }


emptySea : Sea
emptySea =
    Sea { cards = Dict.empty }


size : Sea -> Int
size (Sea { cards }) =
    Dict.size cards


toList : Sea -> List Card.Card
toList (Sea { cards }) =
    Dict.values cards


getDue : Posix -> Sea -> List Card.Card
getDue now (Sea { cards }) =
    cards
        |> Dict.values
        |> List.filter (Card.isDue now)


nextDue : Bool -> Posix -> Sea -> Maybe Card.Card
nextDue allowNew now sea =
    getDue now sea
        |> List.filter (\card -> allowNew || not (FSRS.isNew card.fsrs))
        |> List.sortBy (\card -> Time.posixToMillis card.fsrs.due)
        |> List.head


newCardsToday : Posix -> OpsLog -> Int
newCardsToday now opsLog =
    let
        today =
            FSRS.dateOf now

        firstReviewByCard =
            OpsLog.foldl
                (\op acc ->
                    case op.opKind of
                        ReviewCard { id } ->
                            Dict.update (UUID.toString id)
                                (\existing ->
                                    case existing of
                                        Just t ->
                                            if Time.posixToMillis op.timeStamp < Time.posixToMillis t then
                                                Just op.timeStamp

                                            else
                                                Just t

                                        Nothing ->
                                            Just op.timeStamp
                                )
                                acc

                        _ ->
                            acc
                )
                Dict.empty
                opsLog
    in
    firstReviewByCard
        |> Dict.values
        |> List.filter (\t -> FSRS.dateOf t == today)
        |> List.length


getCard : Card.CardId -> Sea -> Maybe Card.Card
getCard id (Sea { cards }) =
    Dict.get (UUID.toString id) cards


insertCard : Card.Card -> Sea -> Sea
insertCard card (Sea sea) =
    Sea { sea | cards = Dict.insert (UUID.toString card.id) card sea.cards }


removeCard : Card.CardId -> Sea -> Sea
removeCard id (Sea sea) =
    Sea { sea | cards = Dict.remove (UUID.toString id) sea.cards }


updateCard : Card.CardId -> (Card.Card -> Card.Card) -> Sea -> Sea
updateCard id transform sea =
    case getCard id sea of
        Nothing ->
            sea

        Just card ->
            insertCard (transform card) sea


applyOp : Float -> Op -> Sea -> Sea
applyOp desiredRetention op sea =
    case op.opKind of
        CreateCard { id, front, back } ->
            insertCard
                (Card.new id front back op.timeStamp)
                sea

        EditCard { id, front, back } ->
            updateCard id
                (\card -> { card | front = front, back = back })
                sea

        DeleteCard id ->
            removeCard id sea

        ReviewCard { id, rating } ->
            updateCard id
                (Card.review desiredRetention op.timeStamp rating)
                sea

        SetPreamble _ ->
            sea

        SetRetention _ ->
            sea

        AddImage _ ->
            sea


{-| `desiredRetention` (0-1) governs how far out newly-scheduled reviews are
spaced — since it's a live setting rather than something recorded per-op,
replaying history with a different value reschedules every card's next due
date accordingly (matches how the reference app's settings behave).
-}
fromOpsLog : Float -> OpsLog -> Sea
fromOpsLog desiredRetention opsLog =
    OpsLog.foldl (applyOp desiredRetention) emptySea opsLog
