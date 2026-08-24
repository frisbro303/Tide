module Ops.Op exposing (..)

import UUID exposing (UUID)
import Time exposing (Posix)

import Sea.FSRS exposing (Rating)
import Sea.Card exposing (CardId)

type alias Op =
    { id : OpId
    , timeStamp : Posix
    , opKind : OpKind
    }

type OpId
    = OpId UUID

type OpKind
    = CreateCard
        { id : CardId
        , front : String
        , back : String
        }

    | DeleteCard CardId

    | ReviewCard
        { id : CardId
        , rating : Rating
        }
