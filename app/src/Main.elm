module Main exposing (main)

import Browser
import Html exposing (Html, div, text)
import Json.Decode as Decode
import Local.Store as Store
import Sync.Account as Account
import Sync.Session as Session exposing (Session)
import Typst.Typst as Typst


type alias Model =
    { account : Account.Model
    , session : Maybe Session
    , typst : Typst.Model
    }


type Msg
    = AccountMsg Account.Msg
    | TypstMsg Typst.Msg
    | StoreLoaded String Decode.Value


main : Program () Model Msg
main =
    Browser.element
        { init =
            \_ ->
                ( { account = Account.init
                  , session = Nothing
                  , typst = Typst.init
                  }
                , Session.request
                )
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        AccountMsg accountMsg ->
            let
                ( accountModel, cmd, sessionUpdate ) =
                    Account.update accountMsg model.account

                ( newSession, persistCmd ) =
                    case sessionUpdate of
                        Account.SessionEstablished session ->
                            ( Just session, Session.save session )

                        Account.SessionCleared ->
                            ( Nothing, Session.clear )

                        Account.NoSessionChange ->
                            ( model.session, Cmd.none )
            in
            ( { model | account = accountModel, session = newSession }
            , Cmd.batch [ Cmd.map AccountMsg cmd, persistCmd ]
            )

        TypstMsg typstMsg ->
            let
                ( typstModel, cmd ) =
                    Typst.update typstMsg model.typst
            in
            ( { model | typst = typstModel }
            , Cmd.map TypstMsg cmd
            )

        StoreLoaded loadedKey value ->
            case Session.decodeFromStore loadedKey value of
                Just session ->
                    ( model, Cmd.map AccountMsg (Account.refresh session.refreshToken) )

                Nothing ->
                    ( model, Cmd.none )


view : Model -> Html Msg
view model =
    div []
        [ Html.map AccountMsg (Account.view model.session model.account)
        , Html.map TypstMsg (Typst.view model.typst)
        ]


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Sub.map TypstMsg (Typst.subscriptions model.typst)
        , Store.loaded StoreLoaded
        ]
