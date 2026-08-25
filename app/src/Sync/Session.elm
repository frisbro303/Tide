module Sync.Session exposing (Session, clear, decodeFromStore, decoder, request, save)

import Json.Decode as Decode
import Json.Encode as Encode
import Local.Store as Store


key : String
key =
    "session"


type alias Session =
    { accessToken : String
    , refreshToken : String
    , userId : String
    }


decoder : Decode.Decoder Session
decoder =
    Decode.map3 Session
        (Decode.field "access_token" Decode.string)
        (Decode.field "refresh_token" Decode.string)
        (Decode.at [ "user", "id" ] Decode.string)


storeDecoder : Decode.Decoder Session
storeDecoder =
    Decode.map3 Session
        (Decode.field "accessToken" Decode.string)
        (Decode.field "refreshToken" Decode.string)
        (Decode.field "userId" Decode.string)


save : Session -> Cmd msg
save session =
    Store.set key
        (Encode.object
            [ ( "accessToken", Encode.string session.accessToken )
            , ( "refreshToken", Encode.string session.refreshToken )
            , ( "userId", Encode.string session.userId )
            ]
        )


clear : Cmd msg
clear =
    Store.delete key


request : Cmd msg
request =
    Store.get key


decodeFromStore : String -> Decode.Value -> Maybe Session
decodeFromStore loadedKey value =
    if loadedKey == key then
        Decode.decodeValue storeDecoder value |> Result.toMaybe

    else
        Nothing
