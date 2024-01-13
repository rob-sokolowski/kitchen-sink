module Utils exposing (RemoteData(..))


type RemoteData a e
    = Idle
    | Loading
    | Success a
    | Failure e
