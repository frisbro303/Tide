module Sync.Account exposing (Model, Msg(..), SessionUpdate(..), init, refresh, update, view)

import Html exposing (Html, button, div, input, text)
import Html.Attributes exposing (placeholder, type_, value)
import Html.Events exposing (onClick, onInput)
import Http
import Json.Decode as Decode
import Json.Encode as Encode
import Sync.Config exposing (anonKey, supabaseUrl)
import Sync.Session as Session exposing (Session)


type Stage
    = EnteringCredentials
    | AwaitingVerification


type LoginError
    = UnconfirmedEmail
    | OtherLoginError String


type SessionUpdate
    = NoSessionChange
    | SessionEstablished Session
    | SessionCleared


type alias Model =
    { email : String
    , password : String
    , code : String
    , stage : Stage
    , error : Maybe String
    }


type Msg
    = EmailChanged String
    | PasswordChanged String
    | CodeChanged String
    | LoginClicked
    | GotLoginResponse (Result LoginError Session)
    | SignupClicked
    | GotSignupResponse (Result Http.Error ())
    | VerifyClicked
    | GotVerifyResponse (Result Http.Error Session)
    | ResendClicked
    | GotResendResponse (Result Http.Error ())
    | UpdatePasswordClicked String
    | GotUpdatePasswordResponse (Result Http.Error ())
    | LogoutClicked


init : Model
init =
    { email = "", password = "", code = "", stage = EnteringCredentials, error = Nothing }


update : Msg -> Model -> ( Model, Cmd Msg, SessionUpdate )
update msg model =
    case msg of
        EmailChanged email ->
            ( { model | email = email }, Cmd.none, NoSessionChange )

        PasswordChanged password ->
            ( { model | password = password }, Cmd.none, NoSessionChange )

        CodeChanged code ->
            ( { model | code = code }, Cmd.none, NoSessionChange )

        LoginClicked ->
            ( { model | error = Nothing }, login model.email model.password, NoSessionChange )

        GotLoginResponse (Ok session) ->
            ( { model | error = Nothing }, Cmd.none, SessionEstablished session )

        GotLoginResponse (Err UnconfirmedEmail) ->
            ( { model | stage = AwaitingVerification, error = Nothing }
            , resend model.email
            , NoSessionChange
            )

        GotLoginResponse (Err (OtherLoginError message)) ->
            ( { model | error = Just message }, Cmd.none, NoSessionChange )

        SignupClicked ->
            ( { model | error = Nothing }, signup model.email model.password, NoSessionChange )

        GotSignupResponse (Ok ()) ->
            ( { model | stage = AwaitingVerification, error = Nothing }, Cmd.none, NoSessionChange )

        GotSignupResponse (Err _) ->
            ( { model | error = Just "Signup failed" }, Cmd.none, NoSessionChange )

        VerifyClicked ->
            ( { model | error = Nothing }, verify model.email model.code, NoSessionChange )

        GotVerifyResponse (Ok session) ->
            ( { model | error = Nothing }, Cmd.none, SessionEstablished session )

        GotVerifyResponse (Err _) ->
            ( { model | error = Just "Invalid or expired code" }, Cmd.none, NoSessionChange )

        ResendClicked ->
            ( { model | error = Nothing }, resend model.email, NoSessionChange )

        GotResendResponse (Ok _) ->
            ( { model | error = Nothing }, Cmd.none, NoSessionChange )

        GotResendResponse (Err _) ->
            ( { model | error = Just "Could not resend code" }, Cmd.none, NoSessionChange )

        UpdatePasswordClicked accessToken ->
            ( { model | error = Nothing }, updatePassword accessToken model.password, NoSessionChange )

        GotUpdatePasswordResponse (Ok _) ->
            ( { model | error = Nothing }, Cmd.none, NoSessionChange )

        GotUpdatePasswordResponse (Err _) ->
            ( { model | error = Just "Password update failed" }, Cmd.none, NoSessionChange )

        LogoutClicked ->
            ( init, Cmd.none, SessionCleared )


login : String -> String -> Cmd Msg
login email password =
    Http.request
        { method = "POST"
        , headers = [ Http.header "apikey" anonKey ]
        , url = supabaseUrl ++ "/auth/v1/token?grant_type=password"
        , body =
            Http.jsonBody
                (Encode.object
                    [ ( "email", Encode.string email )
                    , ( "password", Encode.string password )
                    ]
                )
        , expect = Http.expectStringResponse GotLoginResponse handleLoginResponse
        , timeout = Nothing
        , tracker = Nothing
        }


refresh : String -> Cmd Msg
refresh refreshToken =
    Http.request
        { method = "POST"
        , headers = [ Http.header "apikey" anonKey ]
        , url = supabaseUrl ++ "/auth/v1/token?grant_type=refresh_token"
        , body =
            Http.jsonBody
                (Encode.object
                    [ ( "refresh_token", Encode.string refreshToken ) ]
                )
        , expect = Http.expectStringResponse GotLoginResponse handleLoginResponse
        , timeout = Nothing
        , tracker = Nothing
        }


type alias AuthErrorBody =
    { errorCode : String }


authErrorDecoder : Decode.Decoder AuthErrorBody
authErrorDecoder =
    Decode.map AuthErrorBody
        (Decode.field "error_code" Decode.string)


handleLoginResponse : Http.Response String -> Result LoginError Session
handleLoginResponse response =
    case response of
        Http.BadStatus_ metadata body ->
            case Decode.decodeString authErrorDecoder body of
                Ok { errorCode } ->
                    if errorCode == "email_not_confirmed" then
                        Err UnconfirmedEmail

                    else
                        Err (OtherLoginError ("Login failed: " ++ errorCode))

                Err _ ->
                    Err (OtherLoginError ("Login failed (" ++ String.fromInt metadata.statusCode ++ ")"))

        Http.GoodStatus_ _ body ->
            case Decode.decodeString Session.decoder body of
                Ok session ->
                    Ok session

                Err _ ->
                    Err (OtherLoginError "Unexpected response from server")

        Http.BadUrl_ _ ->
            Err (OtherLoginError "Bad URL")

        Http.Timeout_ ->
            Err (OtherLoginError "Request timed out")

        Http.NetworkError_ ->
            Err (OtherLoginError "Network error")


signup : String -> String -> Cmd Msg
signup email password =
    Http.request
        { method = "POST"
        , headers = [ Http.header "apikey" anonKey ]
        , url = supabaseUrl ++ "/auth/v1/signup"
        , body =
            Http.jsonBody
                (Encode.object
                    [ ( "email", Encode.string email )
                    , ( "password", Encode.string password )
                    ]
                )
        , expect = Http.expectWhatever GotSignupResponse
        , timeout = Nothing
        , tracker = Nothing
        }


verify : String -> String -> Cmd Msg
verify email code =
    Http.request
        { method = "POST"
        , headers = [ Http.header "apikey" anonKey ]
        , url = supabaseUrl ++ "/auth/v1/verify"
        , body =
            Http.jsonBody
                (Encode.object
                    [ ( "type", Encode.string "signup" )
                    , ( "email", Encode.string email )
                    , ( "token", Encode.string code )
                    ]
                )
        , expect = Http.expectJson GotVerifyResponse Session.decoder
        , timeout = Nothing
        , tracker = Nothing
        }


resend : String -> Cmd Msg
resend email =
    Http.request
        { method = "POST"
        , headers = [ Http.header "apikey" anonKey ]
        , url = supabaseUrl ++ "/auth/v1/resend"
        , body =
            Http.jsonBody
                (Encode.object
                    [ ( "type", Encode.string "signup" )
                    , ( "email", Encode.string email )
                    ]
                )
        , expect = Http.expectWhatever GotResendResponse
        , timeout = Nothing
        , tracker = Nothing
        }


updatePassword : String -> String -> Cmd Msg
updatePassword accessToken newPassword =
    Http.request
        { method = "PUT"
        , headers =
            [ Http.header "apikey" anonKey
            , Http.header "Authorization" ("Bearer " ++ accessToken)
            ]
        , url = supabaseUrl ++ "/auth/v1/user"
        , body =
            Http.jsonBody
                (Encode.object
                    [ ( "password", Encode.string newPassword ) ]
                )
        , expect = Http.expectWhatever GotUpdatePasswordResponse
        , timeout = Nothing
        , tracker = Nothing
        }


view : Maybe Session -> Model -> Html Msg
view maybeSession model =
    case maybeSession of
        Just session ->
            div []
                [ text ("Logged in as " ++ session.userId)
                , button [ onClick LogoutClicked ] [ text "Log out" ]
                ]

        Nothing ->
            case model.stage of
                EnteringCredentials ->
                    div []
                        [ input
                            [ type_ "email"
                            , placeholder "Email"
                            , value model.email
                            , onInput EmailChanged
                            ]
                            []
                        , input
                            [ type_ "password"
                            , placeholder "Password"
                            , value model.password
                            , onInput PasswordChanged
                            ]
                            []
                        , button [ onClick LoginClicked ] [ text "Log in" ]
                        , button [ onClick SignupClicked ] [ text "Sign up" ]
                        , errorView model.error
                        ]

                AwaitingVerification ->
                    div []
                        [ text ("Enter the code sent to " ++ model.email)
                        , input
                            [ type_ "text"
                            , placeholder "Verification code"
                            , value model.code
                            , onInput CodeChanged
                            ]
                            []
                        , button [ onClick VerifyClicked ] [ text "Verify" ]
                        , button [ onClick ResendClicked ] [ text "Resend code" ]
                        , errorView model.error
                        ]


errorView : Maybe String -> Html Msg
errorView error =
    case error of
        Just err ->
            div [] [ text err ]

        Nothing ->
            text ""
