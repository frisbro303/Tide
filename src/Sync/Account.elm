module Sync.Account exposing (Model, Msg(..), SessionUpdate(..), init, refresh, update, view)

import Html exposing (Html, button, div, h3, input, p, span, text)
import Html.Attributes exposing (class, placeholder, type_, value)
import Html.Events exposing (onClick, onInput)
import Http
import Json.Decode as Decode
import Json.Encode as Encode
import Sync.Config exposing (anonKey, supabaseUrl)
import Sync.Session as Session exposing (Session)


type Stage
    = EnteringCredentials
    | AwaitingVerification


type AuthMode
    = Login
    | Signup


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
    , confirmPassword : String
    , code : String
    , stage : Stage
    , mode : AuthMode
    , error : Maybe String
    , newPassword : String
    , newPasswordConfirm : String
    , passwordError : Maybe String
    , passwordSaved : Bool
    , newEmail : String
    , emailError : Maybe String
    , emailSaved : Bool
    }


type Msg
    = EmailChanged String
    | PasswordChanged String
    | ConfirmPasswordChanged String
    | CodeChanged String
    | ModeToggled
    | LoginClicked
    | GotLoginResponse (Result LoginError Session)
    | SignupClicked
    | GotSignupResponse (Result String ())
    | VerifyClicked
    | GotVerifyResponse (Result Http.Error Session)
    | ResendClicked
    | GotResendResponse (Result String ())
    | NewPasswordChanged String
    | NewPasswordConfirmChanged String
    | ChangePasswordClicked String
    | GotUpdatePasswordResponse (Result String ())
    | NewEmailChanged String
    | ChangeEmailClicked String
    | GotUpdateEmailResponse (Result String ())
    | LogoutClicked


init : Model
init =
    { email = ""
    , password = ""
    , confirmPassword = ""
    , code = ""
    , stage = EnteringCredentials
    , mode = Login
    , error = Nothing
    , newPassword = ""
    , newPasswordConfirm = ""
    , passwordError = Nothing
    , passwordSaved = False
    , newEmail = ""
    , emailError = Nothing
    , emailSaved = False
    }


update : Msg -> Model -> ( Model, Cmd Msg, SessionUpdate )
update msg model =
    case msg of
        EmailChanged email ->
            ( { model | email = email }, Cmd.none, NoSessionChange )

        PasswordChanged password ->
            ( { model | password = password }, Cmd.none, NoSessionChange )

        ConfirmPasswordChanged confirmPassword ->
            ( { model | confirmPassword = confirmPassword }, Cmd.none, NoSessionChange )

        CodeChanged code ->
            ( { model | code = code }, Cmd.none, NoSessionChange )

        ModeToggled ->
            ( { model
                | mode =
                    if model.mode == Login then
                        Signup

                    else
                        Login
                , error = Nothing
              }
            , Cmd.none
            , NoSessionChange
            )

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
            if model.password /= model.confirmPassword then
                ( { model | error = Just "Passwords do not match" }, Cmd.none, NoSessionChange )

            else
                ( { model | error = Nothing }, signup model.email model.password, NoSessionChange )

        GotSignupResponse (Ok ()) ->
            ( { model | stage = AwaitingVerification, error = Nothing }, Cmd.none, NoSessionChange )

        GotSignupResponse (Err message) ->
            ( { model | error = Just message }, Cmd.none, NoSessionChange )

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

        GotResendResponse (Err message) ->
            ( { model | error = Just message }, Cmd.none, NoSessionChange )

        NewPasswordChanged newPassword ->
            ( { model | newPassword = newPassword, passwordSaved = False }, Cmd.none, NoSessionChange )

        NewPasswordConfirmChanged newPasswordConfirm ->
            ( { model | newPasswordConfirm = newPasswordConfirm, passwordSaved = False }, Cmd.none, NoSessionChange )

        ChangePasswordClicked accessToken ->
            if model.newPassword /= model.newPasswordConfirm then
                ( { model | passwordError = Just "Passwords do not match" }, Cmd.none, NoSessionChange )

            else
                ( { model | passwordError = Nothing }
                , updatePassword accessToken model.newPassword
                , NoSessionChange
                )

        GotUpdatePasswordResponse (Ok _) ->
            ( { model
                | passwordError = Nothing
                , passwordSaved = True
                , newPassword = ""
                , newPasswordConfirm = ""
              }
            , Cmd.none
            , NoSessionChange
            )

        GotUpdatePasswordResponse (Err message) ->
            ( { model | passwordError = Just message, passwordSaved = False }, Cmd.none, NoSessionChange )

        NewEmailChanged newEmail ->
            ( { model | newEmail = newEmail, emailSaved = False }, Cmd.none, NoSessionChange )

        ChangeEmailClicked accessToken ->
            ( { model | emailError = Nothing }, updateEmail accessToken model.newEmail, NoSessionChange )

        GotUpdateEmailResponse (Ok _) ->
            ( { model | emailError = Nothing, emailSaved = True, newEmail = "" }, Cmd.none, NoSessionChange )

        GotUpdateEmailResponse (Err message) ->
            ( { model | emailError = Just message, emailSaved = False }, Cmd.none, NoSessionChange )

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


type alias ApiErrorBody =
    { errorCode : Maybe String
    , message : Maybe String
    }


apiErrorDecoder : Decode.Decoder ApiErrorBody
apiErrorDecoder =
    Decode.map2 ApiErrorBody
        (Decode.maybe (Decode.field "error_code" Decode.string))
        (Decode.maybe (Decode.field "msg" Decode.string))


handleUnitResponse : Http.Response String -> Result String ()
handleUnitResponse response =
    case response of
        Http.BadStatus_ metadata body ->
            case Decode.decodeString apiErrorDecoder body of
                Ok { errorCode, message } ->
                    Err
                        (message
                            |> orElse errorCode
                            |> Maybe.withDefault ("Request failed (" ++ String.fromInt metadata.statusCode ++ ")")
                        )

                Err _ ->
                    Err ("Request failed (" ++ String.fromInt metadata.statusCode ++ ")")

        Http.GoodStatus_ _ _ ->
            Ok ()

        Http.BadUrl_ _ ->
            Err "Bad URL"

        Http.Timeout_ ->
            Err "Request timed out"

        Http.NetworkError_ ->
            Err "Network error"


orElse : Maybe a -> Maybe a -> Maybe a
orElse fallback maybeValue =
    case maybeValue of
        Just value ->
            Just value

        Nothing ->
            fallback


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
        , expect = Http.expectStringResponse GotSignupResponse handleUnitResponse
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
        , expect = Http.expectStringResponse GotResendResponse handleUnitResponse
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
        , expect = Http.expectStringResponse GotUpdatePasswordResponse handleUnitResponse
        , timeout = Nothing
        , tracker = Nothing
        }


updateEmail : String -> String -> Cmd Msg
updateEmail accessToken newEmail =
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
                    [ ( "email", Encode.string newEmail ) ]
                )
        , expect = Http.expectStringResponse GotUpdateEmailResponse handleUnitResponse
        , timeout = Nothing
        , tracker = Nothing
        }


view : Maybe Session -> Model -> Html Msg
view maybeSession model =
    case maybeSession of
        Just session ->
            div [ class "account-page" ]
                [ accountSection "Account"
                    [ div [ class "account-logged-in" ]
                        [ span [ class "account-muted" ] [ text session.email ]
                        , button [ class "button-ghost", onClick LogoutClicked ] [ text "Log out" ]
                        ]
                    ]
                , accountSection "Email" [ changeEmailForm session model ]
                , accountSection "Password" [ changePasswordForm session model ]
                ]

        Nothing ->
            div [ class "auth" ]
                [ case model.stage of
                    EnteringCredentials ->
                        authForm model

                    AwaitingVerification ->
                        verifyForm model
                ]


accountSection : String -> List (Html Msg) -> Html Msg
accountSection title fields =
    div [ class "settings-section" ]
        [ h3 [ class "history-heading" ] [ text title ]
        , div [ class "settings-fields" ] fields
        ]


changeEmailForm : Session -> Model -> Html Msg
changeEmailForm session model =
    div [ class "settings-field" ]
        [ input
            [ class "auth-input"
            , type_ "email"
            , placeholder "New email"
            , value model.newEmail
            , onInput NewEmailChanged
            ]
            []
        , accountErrorView model.emailError
        , if model.emailSaved then
            p [ class "account-hint" ] [ text "Check your new email to confirm the change" ]

          else
            text ""
        , div [ class "account-form-actions" ]
            [ button
                [ class "button-ghost"
                , onClick (ChangeEmailClicked session.accessToken)
                ]
                [ text "Update email" ]
            ]
        ]


changePasswordForm : Session -> Model -> Html Msg
changePasswordForm session model =
    div [ class "settings-field" ]
        [ input
            [ class "auth-input"
            , type_ "password"
            , placeholder "New password"
            , value model.newPassword
            , onInput NewPasswordChanged
            ]
            []
        , input
            [ class "auth-input"
            , type_ "password"
            , placeholder "Confirm new password"
            , value model.newPasswordConfirm
            , onInput NewPasswordConfirmChanged
            ]
            []
        , accountErrorView model.passwordError
        , if model.passwordSaved then
            p [ class "account-hint" ] [ text "Password updated" ]

          else
            text ""
        , div [ class "account-form-actions" ]
            [ button
                [ class "button-ghost"
                , onClick (ChangePasswordClicked session.accessToken)
                ]
                [ text "Update password" ]
            ]
        ]


accountErrorView : Maybe String -> Html Msg
accountErrorView error =
    case error of
        Just err ->
            p [ class "account-error" ] [ text err ]

        Nothing ->
            text ""


authForm : Model -> Html Msg
authForm model =
    let
        modeLabel =
            case model.mode of
                Login ->
                    "Log in"

                Signup ->
                    "Sign up"
    in
    div [ class "auth-card" ]
        [ p [ class "auth-title" ] [ text modeLabel ]
        , div [ class "auth-form" ]
            ([ input
                [ class "auth-input"
                , type_ "email"
                , placeholder "Email"
                , value model.email
                , onInput EmailChanged
                ]
                []
             , input
                [ class "auth-input"
                , type_ "password"
                , placeholder "Password"
                , value model.password
                , onInput PasswordChanged
                ]
                []
             ]
                ++ (case model.mode of
                        Login ->
                            []

                        Signup ->
                            [ input
                                [ class "auth-input"
                                , type_ "password"
                                , placeholder "Confirm password"
                                , value model.confirmPassword
                                , onInput ConfirmPasswordChanged
                                ]
                                []
                            ]
                   )
                ++ [ errorView model.error
                   , button
                        [ class "button-primary"
                        , onClick
                            (case model.mode of
                                Login ->
                                    LoginClicked

                                Signup ->
                                    SignupClicked
                            )
                        ]
                        [ text modeLabel ]
                   ]
            )
        , button [ class "auth-switch", onClick ModeToggled ]
            [ text
                (case model.mode of
                    Login ->
                        "Need an account? Sign up"

                    Signup ->
                        "Already have an account? Log in"
                )
            ]
        ]


verifyForm : Model -> Html Msg
verifyForm model =
    div [ class "auth-card" ]
        [ p [ class "auth-title" ] [ text "Confirm your email" ]
        , p [ class "auth-hint" ] [ text ("Enter the code sent to " ++ model.email) ]
        , div [ class "auth-form" ]
            [ input
                [ class "auth-input"
                , type_ "text"
                , placeholder "Verification code"
                , value model.code
                , onInput CodeChanged
                ]
                []
            , errorView model.error
            , button [ class "button-primary", onClick VerifyClicked ] [ text "Verify" ]
            ]
        , button [ class "auth-switch", onClick ResendClicked ] [ text "Resend code" ]
        ]


errorView : Maybe String -> Html Msg
errorView error =
    case error of
        Just err ->
            p [ class "auth-error" ] [ text err ]

        Nothing ->
            text ""
