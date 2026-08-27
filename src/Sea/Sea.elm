module Sea.Sea exposing (Counts, Sea, applyOp, counts, emptySea, fromOpsLog, getCard, getDue, insertCard, nextDue, removeCard, size, toList, updateCard)

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


nextDue : Posix -> Sea -> Maybe Card.Card
nextDue now sea =
    getDue now sea
        |> List.sortBy (\card -> Time.posixToMillis card.fsrs.due)
        |> List.head


type alias Counts =
    { new : Int, due : Int }


counts : Posix -> Sea -> Counts
counts now sea =
    let
        due =
            getDue now sea

        newCount =
            List.length (List.filter (\card -> FSRS.isNew card.fsrs) due)
    in
    { new = newCount, due = List.length due - newCount }


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
                (Card.Card id front back (FSRS.initialState op.timeStamp))
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


{-| `desiredRetention` (0-1) governs how far out newly-scheduled reviews are
spaced — since it's a live setting rather than something recorded per-op,
replaying history with a different value reschedules every card's next due
date accordingly (matches how the reference app's settings behave).
-}
fromOpsLog : Float -> OpsLog -> Sea
fromOpsLog desiredRetention opsLog =
    OpsLog.foldl (applyOp desiredRetention) emptySea opsLog
