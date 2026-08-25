module Sea.Sea exposing (..)

import Dict exposing (Dict)
import Ops.Op exposing (Op, OpKind(..))
import Ops.OpsLog exposing (OpsLog)
import Sea.Card as Card
import Sea.FSRS as FSRS
import Time exposing (Posix)
import UUID


type alias Sea =
    { cards : Dict String Card.Card
    }


emptySea : Sea
emptySea =
    { cards = Dict.empty
    }


getDue : Posix -> Sea -> List Card.Card
getDue now sea =
    sea.cards
        |> Dict.values
        |> List.filter (Card.isDue now)


getCard : Card.CardId -> Sea -> Maybe Card.Card
getCard id sea =
    Dict.get (UUID.toString id) sea.cards


insertCard : Card.Card -> Sea -> Sea
insertCard card sea =
    { sea
        | cards =
            Dict.insert (UUID.toString card.id) card sea.cards
    }


removeCard : Card.CardId -> Sea -> Sea
removeCard id sea =
    { sea
        | cards =
            Dict.remove (UUID.toString id) sea.cards
    }


updateCard : Card.CardId -> (Card.Card -> Card.Card) -> Sea -> Sea
updateCard id transform sea =
    case getCard id sea of
        Nothing ->
            sea

        Just card ->
            insertCard (transform card) sea


applyOp : Op -> Sea -> Sea
applyOp op sea =
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
                (Card.review op.timeStamp rating)
                sea


fromOpsLog : OpsLog -> Sea
fromOpsLog opsLog =
    List.foldl applyOp emptySea opsLog
