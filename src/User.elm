module User exposing (Role(..), Session(..), User, UserId)

import Time


type alias User =
    -- TODO: Make this a custom type, variants by OAuth vs other methods???
    { id : UserId
    , realname : String
    , username : String
    , email : String

    -- TODO: I don't think we'll want password, not sure what Jim's plans were here
    , password : String

    -- TODO: Implement "nowish" pattern on the backend???
    , createdAt : Time.Posix
    , updatedAt : Time.Posix
    , role : Role
    }


type Role
    = AdminRole
    | UserRole



-- TODO: Lamdera internal sessionId?


type SessionId
    = SessionId


type alias UserId =
    -- TODO: Should this be an int??? How does this get generated?
    Int


type Session
    = GuestSession SessionId
    | UserSession SessionId UserId Time.Posix
