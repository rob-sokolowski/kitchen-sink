module Types exposing
    ( AdminDisplay(..)
    , BackendModel
    , BackendMsg(..)
    , FrontendModel(..)
    , FrontendMsg(..)
    , InitData2
    , LoadedModel
    , LoadingModel
    , SignInState(..)
    , ToBackend(..)
    , ToFrontend(..)
    )

import AssocList
import Auth.Common
import Browser exposing (UrlRequest)
import Browser.Navigation exposing (Key)
import Dict exposing (Dict)
import Http
import Id exposing (Id)
import KeyValueStore
import Lamdera exposing (ClientId, SessionId)
import LocalUUID
import Postmark exposing (PostmarkSendResponse)
import Route exposing (Route)
import Stripe.Codec
import Stripe.Product
import Stripe.PurchaseForm exposing (PurchaseForm, PurchaseFormValidated)
import Stripe.Stripe exposing (Price, PriceData, PriceId, ProductId, StripeSessionId)
import Time
import Untrusted exposing (Untrusted)
import Url exposing (Url)
import User exposing (Session, User, UserId)
import Weather


type FrontendModel
    = Loading LoadingModel
    | Loaded LoadedModel


type alias LoadingModel =
    { key : Key
    , now : Time.Posix
    , window : Maybe { width : Int, height : Int }
    , route : Route
    , initData : Maybe InitData2
    }


type alias LoadedModel =
    { key : Key
    , now : Time.Posix
    , window : { width : Int, height : Int }
    , showTooltip : Bool

    -- STRIPE
    , prices : AssocList.Dict (Id ProductId) { priceId : Id PriceId, price : Price }
    , productInfoDict : AssocList.Dict (Id ProductId) Stripe.Stripe.ProductInfo
    , selectedProduct : Maybe ( Id ProductId, Id PriceId, Stripe.Product.Product_ )
    , form : PurchaseForm

    -- USER
    , currentUser : Maybe User.User
    , signInState : SignInState
    , realname : String
    , username : String
    , email : String
    , password : String
    , passwordConfirmation : String

    -- ADMIN
    , adminDisplay : AdminDisplay
    , backendModel : Maybe BackendModel

    --
    , route : Route
    , message : String

    -- EXAMPLES
    , weatherData : Maybe Weather.WeatherData
    , inputCity : String

    -- DATA (JC)
    , keyValueStore : Dict.Dict String String
    , inputKey : String
    , inputValue : String
    , inputFilterData : String
    , kvViewType : KeyValueStore.KVViewType
    }



-- TODO: Where should this User type def live?
-- TODO: Also, see Jim's User type in User.elm.. it's a bit more complete


type SignInState
    = SignedOut
    | SignUp
    | SignedIn


type AdminDisplay
    = ADStripe
    | ADUser
    | ADKeyValues


type alias BackendModel =
    { randomAtmosphericNumbers : Maybe (List Int)
    , localUuidData : Maybe LocalUUID.Data

    -- USER
    , userDictionary : Dict.Dict UserId User

    -- TODO: Jim's placeholder had a BiDict, that might be a better fit that a standard Dict?
    --       But I'm unsure if a key can be a record type???
    , sessions : Dict SessionId Session -- sessionId to username
    , pendingAuths : Dict SessionId Auth.Common.PendingAuth

    --STRIPE
    , orders : AssocList.Dict (Id StripeSessionId) Stripe.Codec.Order
    , pendingOrder : AssocList.Dict (Id StripeSessionId) Stripe.Codec.PendingOrder
    , expiredOrders : AssocList.Dict (Id StripeSessionId) Stripe.Codec.PendingOrder
    , prices : AssocList.Dict (Id ProductId) Stripe.Codec.Price2
    , time : Time.Posix
    , products : Stripe.Stripe.ProductInfoDict

    -- EXPERIMENTAL
    , keyValueStore : Dict.Dict String String
    }


type FrontendMsg
    = NoOp
    | UrlClicked UrlRequest
    | UrlChanged Url
    | Tick Time.Posix
    | GotWindowSize Int Int
    | PressedShowTooltip
    | MouseDown
      -- STRIPE
    | BuyProduct (Id ProductId) (Id PriceId) Stripe.Product.Product_
    | PressedSelectTicket (Id ProductId) (Id PriceId)
    | FormChanged PurchaseForm
    | PressedSubmitForm (Id ProductId) (Id PriceId)
    | PressedCancelForm
    | AskToRenewPrices
      -- USER
    | SignIn
    | SetSignInState SignInState
    | SubmitSignIn
    | SignInWithGoogle
    | SubmitSignOut
    | SubmitSignUp
    | InputRealname String
    | InputUsername String
    | InputEmail String
    | InputPassword String
    | InputPasswordConfirmation String
      -- ADMIN
    | SetAdminDisplay AdminDisplay
      --
    | SetViewport
      -- EXAMPLES
    | CopyTextToClipboard String
    | Chirp
    | RequestWeatherData String
    | InputCity String
      -- DATA (JC)
    | InputKey String
    | InputValue String
    | InputFilterData String
    | AddKeyValuePair String String
    | GetValueWithKey String
    | GotValue (Result Http.Error String)
    | DataUploaded (Result Http.Error ())
    | SetKVViewType KeyValueStore.KVViewType


type ToBackend
    = SubmitFormRequest (Id PriceId) (Untrusted PurchaseFormValidated)
    | CancelPurchaseRequest
    | AdminInspect (Maybe User.User)
      -- STRIPE
    | RenewPrices
      -- USER
    | SignInRequest String String
    | SignOutRequest String
    | SignUpRequest String String String String -- realname, username, email, password
      -- EXAMPLES
    | GetWeatherData String
      -- Auth
    | Auth_ToBackend Auth.Common.ToBackend



-- Or is it an int???


type BackendMsg
    = GotTime Time.Posix
      --
    | GotAtmosphericRandomNumbers (Result Http.Error String)
      -- STRIPE
    | GotPrices (Result Http.Error (List PriceData))
    | GotPrices2 ClientId (Result Http.Error (List PriceData))
    | OnConnected SessionId ClientId
    | CreatedCheckoutSession SessionId ClientId (Id PriceId) PurchaseFormValidated (Result Http.Error ( Id StripeSessionId, Time.Posix ))
    | ExpiredStripeSession (Id StripeSessionId) (Result Http.Error ())
    | ConfirmationEmailSent (Id StripeSessionId) (Result Http.Error PostmarkSendResponse)
    | ErrorEmailSent (Result Http.Error PostmarkSendResponse)
      -- EXAMPLES
    | GotWeatherData ClientId (Result Http.Error Weather.WeatherData)
      -- Auth
    | Auth_BackendMsg Auth.Common.BackendMsg
    | Auth_RenewSession UserId SessionId ClientId Time.Posix


type alias InitData2 =
    { prices : AssocList.Dict (Id ProductId) { priceId : Id PriceId, price : Price }
    , productInfo : AssocList.Dict (Id ProductId) Stripe.Stripe.ProductInfo
    }


type ToFrontend
    = InitData InitData2
    | GotMessage String
    | SubmitFormResponse (Result String (Id StripeSessionId))
    | AdminInspectResponse BackendModel
      -- Auth
    | Auth_ToFrontend Auth.Common.ToFrontend
    | Auth_ActiveSession User
      -- USER
    | UserSignedIn (Maybe User)
      -- EXAMPLE
    | ReceivedWeatherData (Result Http.Error Weather.WeatherData)
      -- DATA (JC)
    | GotKeyValueStore (Dict.Dict String String)



-- STRIPE
