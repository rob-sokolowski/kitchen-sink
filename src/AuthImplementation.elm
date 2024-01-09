module AuthImplementation exposing (..)

<<<<<<< HEAD
=======
-- TODO: IS there a config???
--import Config

>>>>>>> 76e3cf2a9aa6812ef8dd5d112fa5e6211eb3adb2
import Auth.Common
import Auth.Flow
import Auth.Method.OAuthGithub
import Auth.Method.OAuthGoogle
<<<<<<< HEAD
import Config
import Dict
import Env

import Lamdera
import Pages.Login.Provider_.Callback
import RemoteData exposing (RemoteData(..))
import Secrets
import Task
import Time
import Types exposing (BackendModel, BackendMsg(..), FrontendModel, FrontendMsg(..), ToFrontend(..))
import User exposing (Session(..), User)
=======
import Dict
import Dict.Extra as Dict
import Env
import Lamdera
import Task
import Time
import Types exposing (BackendModel, BackendMsg(..), FrontendModel, FrontendMsg(..), ToBackend(..), ToFrontend(..), UserId)
import User exposing (User)
>>>>>>> 76e3cf2a9aa6812ef8dd5d112fa5e6211eb3adb2


config : Auth.Common.Config FrontendMsg ToBackend BackendMsg ToFrontend FrontendModel BackendModel
config =

    { toBackend = Auth_ToBackend
    , toFrontend = Auth_BackendMsg
    , backendMsg = Auth_BackendMsg
    , sendToFrontend = Lamdera.sendToFrontend
    , sendToBackend = Lamdera.sendToBackend
    , methods =
        [ Auth.Method.OAuthGoogle.configuration googleOAuthClientId googleOAuthClientSecret

        --, Auth.Method.OAuthGithub.configuration Secrets.githubOAuthClientId Secrets.githubOAuthClientSecret

        ]
    , renewSession = renewSession
    , logout = logout
    }


backendConfig model =

    { asToFrontend = Auth_ToFrontend
    , asBackendMsg = Auth_BackendMsg

    , sendToFrontend = Lamdera.sendToFrontend
    , backendModel = model
    , loadMethod = Auth.Flow.methodLoader config.methods
    , handleAuthSuccess = handleAuthSuccess model
    , isDev = Env.mode == Env.Development
    , renewSession = renewSession
    , logout = logout
    }


updateFromBackend authToFrontendMsg model =
    case authToFrontendMsg of
        Auth.Common.AuthInitiateSignin url ->
            Auth.Flow.startProviderSignin url model

        Auth.Common.AuthError err ->
            Auth.Flow.setError model err

        Auth.Common.AuthSessionChallenge reason ->
            -- TODO: I don't know what this is, but Mario's code had this todo statement, I'm putting a noop here in the meantime
            --Debug.todo "Auth.Common.AuthSessionChallenge"
            ( model, Cmd.none )


handleAuthSuccess :
    BackendModel
    -> Lamdera.SessionId
    -> Lamdera.ClientId
    -> Auth.Common.UserInfo
    -> Maybe Auth.Common.Token
    -> Time.Posix
    -> ( BackendModel, Cmd BackendMsg )
handleAuthSuccess backendModel sessionId clientId userInfo authToken now =
    let
<<<<<<< HEAD
        renewSession_ : User.UserId -> Lamdera.SessionId -> Lamdera.ClientId -> Cmd BackendMsg
        renewSession_ email sid cid =
            Task.perform (RenewSession email sid cid) Time.now
=======
        renewSession_ : UserId -> Lamdera.SessionId -> Lamdera.ClientId -> Cmd BackendMsg
        renewSession_ email sid cid =
            Task.perform (Auth_RenewSession email sid cid) Time.now
>>>>>>> 76e3cf2a9aa6812ef8dd5d112fa5e6211eb3adb2
    in
    if backendModel.users |> Dict.any (\k u -> u.email == userInfo.email) then
        let
            ( response, cmd ) =
                backendModel.users
                    |> Dict.find (\k u -> u.email == userInfo.email)
                    |> Maybe.map
                        (\( k, u ) ->
                            ( Success u, renewSession_ u.id sessionId clientId )
                        )
                    |> Maybe.withDefault ( Failure "email or password is invalid", Cmd.none )
        in
        ( backendModel
        , Cmd.batch
            [ Lamdera.sendToFrontend clientId (Deliver_AuthResult response)
            , cmd
            ]
        )

    else
        let
            user_ : User
            user_ =
                { id = Dict.size backendModel.users
                , email = userInfo.email
                }

            response : RemoteData e User
            response =
                Success user_
        in
        ( { backendModel | users = backendModel.users |> Dict.insert user_.id user_ }
        , Cmd.batch
            [ renewSession_ user_.id sessionId clientId
            , Lamdera.sendToFrontend clientId (Deliver_AuthResult response)
            ]
        )


renewSession : String -> String -> BackendModel -> ( BackendModel, Cmd BackendMsg )
renewSession sessionId clientId model =
    model
        |> getSessionUser sessionId
        |> Maybe.map (\user -> ( model, Lamdera.sendToFrontend clientId (ActiveSession user) ))
        |> Maybe.withDefault ( model, Cmd.none )


logout sessionId clientId model =
    ( { model | sessions = model.sessions |> Dict.remove sessionId }, Cmd.none )


getSessionUser : Lamdera.SessionId -> BackendModel -> Maybe User
getSessionUser sid model =
    model.sessions
        |> Dict.get sid
        |> Maybe.andThen
            (\session ->
                case session of
                    GuestSession _ ->
                        Nothing

                    UserSession _ uid _ ->
                        Dict.get uid model.users
            )
<<<<<<< HEAD
=======



-- begin region: auth secrets config


googleOAuthClientId : String
googleOAuthClientId =
    case Env.mode of
        Env.Production ->
            -- this secret is saved in Lamdera admin dashboard
            -- TODO: GCP console!
            "enjoy the holidays!"

        _ ->
            -- Safe to commit publicly, this is only for local dev
            "happy new year!"


googleOAuthClientSecret : String
googleOAuthClientSecret =
    case Env.mode of
        Env.Production ->
            -- this secret is saved in Lamdera admin dashboard
            -- TODO: GCP console!
            "happy hanukkah!"

        _ ->
            -- safe to commit publicly, this is only for local dev
            "merry christmas!"



-- TODO: GitHub Example???
--       Jim specified Google, but I think we should provide GitHub as well!
-- end region: auth secrests config
>>>>>>> 76e3cf2a9aa6812ef8dd5d112fa5e6211eb3adb2
