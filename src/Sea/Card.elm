module Sea.Card exposing (..)

import UUID exposing (UUID)
import Time exposing (Posix)

import Sea.FSRS as FSRS exposing (Rating)

type alias CardId =
    UUID


type alias Card =
    { id : CardId
    , front : String
    , back : String
    , fsrs : FSRS.State
    }

review : Posix -> Rating -> Card -> Card
review now rating card =
    { card
        | fsrs = FSRS.review now rating card.fsrs
    }

isDue : Posix -> Card -> Bool
isDue now card =
    Time.posixToMillis card.fsrs.due
        <= Time.posixToMillis now
