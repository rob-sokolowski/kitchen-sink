module AuthImplementation exposing (backendConfig, config, updateFromBackend)

import Auth.Common exposing (MethodId)
import Auth.Flow
import Auth.Method.OAuthGoogle
import Dict
import Dict.Extra as Dict
import Env
import Lamdera exposing (ClientId, SessionId)
import Task
import Time
import Types
    exposing
        ( BackendModel
        , BackendMsg(..)
        , FrontendMsg(..)
        , LoadedModel
        , ToBackend(..)
        , ToFrontend(..)
        )
import User exposing (User)


googleOAuthClientId : String
googleOAuthClientId =
    "740743074280-dnv5r7rai8plqsfgg2sj0em2cg4q2rvd.apps.googleusercontent.com"


googleOAuthClientSecret : String
googleOAuthClientSecret =
    -- TODO: For local dev this should be safe to commit... confirm this??
    "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"


config : Auth.Common.Config FrontendMsg ToBackend BackendMsg ToFrontend LoadedModel BackendModel
config =
    -- TODO: I believe I have to refactor to accept the custom type FrontendModel and not the LoadedModel record
    --       in order to properly handle loading case???
    { toBackend = Auth_ToBackend
    , toFrontend = Auth_ToFrontend
    , backendMsg = Auth_BackendMsg
    , sendToFrontend = Lamdera.sendToFrontend
    , sendToBackend = Lamdera.sendToBackend
    , methods =
        [ Auth.Method.OAuthGoogle.configuration googleOAuthClientId googleOAuthClientSecret

        -- TODO: GitHub provider
        --, Auth.Method.OAuthGithub.configuration Config.githubOAuthClientId Config.githubOAuthClientSecret
        ]
    , renewSession = renewSession

    --, logout = logout
    }


backendConfig :
    BackendModel
    ->
        { asToFrontend : Auth.Common.ToFrontend -> ToFrontend
        , asBackendMsg : Auth.Common.BackendMsg -> BackendMsg
        , sendToFrontend : ClientId -> ToFrontend -> Cmd backendMsg
        , backendModel : BackendModel
        , loadMethod : Auth.Common.MethodId -> Maybe (Auth.Common.Method FrontendMsg BackendMsg LoadedModel BackendModel)
        , handleAuthSuccess : SessionId -> ClientId -> Auth.Common.UserInfo -> MethodId -> Maybe Auth.Common.Token -> Time.Posix -> ( BackendModel, Cmd BackendMsg )
        , isDev : Bool
        , renewSession : String -> String -> BackendModel -> ( BackendModel, Cmd BackendMsg )
        , logout : String -> String -> BackendModel -> ( BackendModel, Cmd BackendMsg )
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


logout : String -> String -> BackendModel -> ( BackendModel, Cmd BackendMsg )
logout sessionId clientId model =
    -- TODO: Actually implement logout, here's the old one
    --    ( { model | sessions = model.sessions |> Dict.remove sessionId }, Cmd.none )
    ( model, Cmd.none )


updateFromBackend :
    Auth.Common.ToFrontend
    -> LoadedModel
    -> ( LoadedModel, Cmd msg )
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
    -> MethodId
    -> Maybe Auth.Common.Token
    -> Time.Posix
    -> ( BackendModel, Cmd BackendMsg )
handleAuthSuccess backendModel sessionId clientId userInfo methodId authToken now =
    -- TODO: It seems odd to me that I'm not using methodId, authToken, nor now..
    let
        renewSession_ : String -> SessionId -> ClientId -> Cmd BackendMsg
        renewSession_ email sid cid =
            -- TODO: This "renewSession" name collides with the outer scope.. but they're doing different things..
            -- it appears there's some intended hook for "renewSession", as it is a field in the Auth.BackendConfig record
            -- so why do we mention it again here instead of using that config??
            Task.perform (Auth_RenewSession email sid cid) Time.now

        doesUserExist : Bool
        doesUserExist =
            -- TODO: The implementation I'm following does a Dict.any, and if there is a hit, a subsequent Dict.find
            --       Is this more performant than doing a Dict.find to begin with??
            --       Another thing is Dict.find returns maybe user, which might simplify the truthy-case below
            Dict.any (\_ u -> u.email == userInfo.email) backendModel.userDictionary
    in
    -- If there exists a user with this email in our known users dictionary, we'll renew their session on the backend
    if doesUserExist then
        let
            tmpPackaged : ( Maybe User, Cmd BackendMsg )
            tmpPackaged =
                backendModel.userDictionary
                    |> Dict.find (\_ u -> u.email == userInfo.email)
                    |> Maybe.map
                        (\( _, u ) ->
                            ( Just u, renewSession_ u.id sessionId clientId )
                        )
                    |> Maybe.withDefault ( Nothing, Cmd.none )

            ( user, renewCmd ) =
                tmpPackaged

            sendToFrontEndMsg : Cmd BackendMsg
            sendToFrontEndMsg =
                case user of
                    Just user_ ->
                        Lamdera.sendToFrontend clientId (Auth_ActiveSession user_)

                    Nothing ->
                        Cmd.none
        in
        ( backendModel
        , Cmd.batch
            [ -- TODO: sendToFrontend with new redirect page (yet to be coded up)
              sendToFrontEndMsg
            , renewCmd
            ]
        )

    else
        -- If we're here, we've had a successful authentication for a user we don't have saved in our userDictionary
        -- create new user record to both return to the frontend as well update our backendModel to include this new
        -- user record.
        let
            user_ : User
            user_ =
                -- TODO: Default new user helper function
                { realname = "Jim Carlson"
                , username = "jxxcarlson"
                , email = "jxxcarlson@gmail.com"
                , password = "1234"
                , id = "661b76d8-eee8-42fb-a28d-cf8ada73f869"
                , created_at = Time.millisToPosix 1704237963000
                , updated_at = Time.millisToPosix 1704237963000
                , role = User.UserRole
                }
        in
        ( { backendModel | userDictionary = Dict.insert user_.id user_ backendModel.userDictionary }
        , Cmd.batch
            [ renewSession_ user_.id sessionId clientId
            , Lamdera.sendToFrontend clientId (Auth_ActiveSession user_)
            ]
        )


renewSession : String -> String -> BackendModel -> ( BackendModel, Cmd BackendMsg )
renewSession sessionId clientId model =
    model
        |> getSessionUser sessionId
        |> Maybe.map (\user -> ( model, Lamdera.sendToFrontend clientId (Auth_ActiveSession user) ))
        |> Maybe.withDefault ( model, Cmd.none )


getSessionUser : Lamdera.SessionId -> BackendModel -> Maybe User
getSessionUser sid model =
    model.sessions
        |> Dict.get sid
        |> Maybe.andThen (\session -> model.userDictionary |> Dict.get session.userId)
