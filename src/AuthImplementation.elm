module AuthImplementation exposing (..)

import Auth.Common
import Auth.Flow
import Auth.Method.OAuthGithub
import Auth.Method.OAuthGoogle
import Dict
import Dict.Extra as Dict
import Env
import Lamdera
import Task
import Time
import Types
    exposing
        ( BackendModel
        , BackendMsg(..)
        , FrontendModel
        , FrontendMsg(..)
        , ToBackend(..)
        , ToFrontend(..)
        )
import User exposing (Role(..), Session(..), User, UserId)


config : Auth.Common.Config FrontendMsg ToBackend BackendMsg ToFrontend FrontendModel BackendModel
config =
    { toBackend = Auth_ToBackend
    , toFrontend = Auth_ToFrontend
    , backendMsg = Auth_BackendMsg
    , sendToFrontend = Lamdera.sendToFrontend
    , sendToBackend = Lamdera.sendToBackend
    , methods =
        [ Auth.Method.OAuthGoogle.configuration googleOAuthClientId googleOAuthClientSecret

        --, Auth.Method.OAuthGithub.configuration Secrets.githubOAuthClientId Secrets.githubOAuthClientSecret
        ]
    , renewSession = renewSession

    -- TODO: Why do I need to comment out this logout? Relic of old implementation? Where did it go?
    --, logout = logout
    }



--backendConfig :
--    BackendModel
--    ->
--        { asToFrontend : Auth.Common.ToFrontend -> ToFrontend
--        , asBackendMsg : Auth.Common.BackendMsg -> BackendMsg
--        , sendToFrontend : Lamdera.ClientId -> toFrontend -> Cmd backendMsg
--        , backendModel : BackendModel
--        , loadMethod : Auth.Common.MethodId -> Maybe (Auth.Common.Method FrontendMsg BackendMsg FrontendModel BackendModel)
--        , handleAuthSuccess : Lamdera.SessionId -> Lamdera.ClientId -> Auth.Common.UserInfo -> Maybe Auth.Common.Token -> Time.Posix -> ( BackendModel, Cmd BackendMsg )
--        , isDev : Bool
--        , renewSession : String -> String -> BackendModel -> ( BackendModel, Cmd BackendMsg )
--
--        --, logout : comparable -> b -> { a | sessions : Dict.Dict comparable v } -> ( { a | sessions : Dict.Dict comparable v }, Cmd msg )
--        }


backendConfig backendModel =
    { asToFrontend = Auth_ToFrontend
    , asBackendMsg = Auth_BackendMsg
    , sendToFrontend = Lamdera.sendToFrontend
    , backendModel = backendModel
    , loadMethod = Auth.Flow.methodLoader config.methods
    , handleAuthSuccess = handleAuthSuccess backendModel
    , isDev = Env.mode == Env.Development
    , renewSession = renewSession

    --, logout = logout
    }


updateFromBackend :
    Auth.Common.ToFrontend
    -> { frontendModel | authFlow : Auth.Common.Flow }
    -> ( { frontendModel | authFlow : Auth.Common.Flow }, Cmd msg )
updateFromBackend authToFrontendMsg model =
    case authToFrontendMsg of
        Auth.Common.AuthInitiateSignin url ->
            Auth.Flow.startProviderSignin url model

        Auth.Common.AuthError err ->
            Auth.Flow.setError model err



--Auth.Common.AuthSessionChallenge reason ->
--    -- TODO: I don't know what this is, but Mario's code had this todo statement, I'm putting a noop here in the meantime
--    --Debug.todo "Auth.Common.AuthSessionChallenge"
--    ( model, Cmd.none )
--type Idk
--    = Success UserId
--    | Failure String


handleAuthSuccess :
    BackendModel
    -> Lamdera.SessionId
    -> Lamdera.ClientId
    -> Auth.Common.UserInfo
    -> Maybe Auth.Common.Token
    -> Time.Posix
    -> ( BackendModel, Cmd BackendMsg )
handleAuthSuccess backendModel sessionId clientId userInfo authToken now =
    -- TODO: Very strange compiler bug??
    -- noop for now, but this was the original function
    ( backendModel, Cmd.none )



--let
--    renewSession_ : UserId -> Lamdera.SessionId -> Lamdera.ClientId -> Cmd BackendMsg
--    renewSession_ email sid cid =
--        Task.perform (Auth_RenewSession email sid cid) Time.now
--in
--if backendModel.userDictionary |> Dict.any (\k u -> u.email == userInfo.email) then
--    let
--        ( response, cmd ) =
--            backendModel.userDictionary
--                |> Dict.find (\k u -> u.email == userInfo.email)
--                |> Maybe.map
--                    (\( k, u ) ->
--                        ( Success 123, renewSession_ u.id sessionId clientId )
--                    )
--                |> Maybe.withDefault ( Failure "email or password is invalid", Cmd.none )
--
--        -- TODO: Clean this up!!
--        response_ =
--            case response of
--                Success user ->
--                    Just user
--
--                _ ->
--                    Nothing
--    in
--    ( backendModel
--    , Cmd.batch
--        [ cmd
--        , case response_ of
--            Just user ->
--                Lamdera.sendToFrontend clientId (Auth_ActiveSession user)
--
--            Nothing ->
--                Cmd.none
--        ]
--    )
--
--else
--    let
--        user_ : User
--        user_ =
--            { id = Dict.size backendModel.userDictionary
--            , realname = ""
--            , username = ""
--            , email = userInfo.email
--
--            -- TODO: I don't think we'll want password, not sure what Jim's plans were here
--            , password = ""
--            , createdAt = Time.millisToPosix 1704237963000
--            , updatedAt = Time.millisToPosix 1704237963000
--            , role = UserRole
--            }
--    in
--    ( { backendModel
--        | userDictionary = backendModel.userDictionary |> Dict.insert user_.id user_
--      }
--    , Cmd.batch
--        [ renewSession_ user_.id sessionId clientId
--        , Lamdera.sendToFrontend clientId (Auth_ActiveSession user_)
--        ]
--    )


renewSession : String -> String -> BackendModel -> ( BackendModel, Cmd BackendMsg )
renewSession sessionId clientId model =
    model
        |> getSessionUser sessionId
        |> Maybe.map (\user -> ( model, Lamdera.sendToFrontend clientId (Auth_ActiveSession user) ))
        |> Maybe.withDefault ( model, Cmd.none )


logout sessionId clientId model =
    ( { model | sessions = model.sessions |> Dict.remove sessionId }, Cmd.none )



-- TODO: Where should User defs live??


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
                        Dict.get uid model.userDictionary
            )



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
