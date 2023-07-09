module Evergreen.V56.PurchaseForm exposing (..)

import Evergreen.V56.EmailAddress
import Evergreen.V56.Name
import Evergreen.V56.TravelMode
import String.Nonempty


type PressedSubmit
    = PressedSubmit
    | NotPressedSubmit


type SubmitStatus
    = NotSubmitted PressedSubmit
    | Submitting
    | SubmitBackendError String


type alias PurchaseForm =
    { submitStatus : SubmitStatus
    , attendee1Name : String
    , attendee2Name : String
    , billingEmail : String
    , country : String
    , originCity : String
    , primaryModeOfTravel : Maybe Evergreen.V56.TravelMode.TravelMode
    , grantContribution : String
    , grantApply : Bool
    , sponsorship : Maybe String
    }


type alias SinglePurchaseData =
    { attendeeName : Evergreen.V56.Name.Name
    , billingEmail : Evergreen.V56.EmailAddress.EmailAddress
    , country : String.Nonempty.NonemptyString
    , originCity : String.Nonempty.NonemptyString
    , primaryModeOfTravel : Evergreen.V56.TravelMode.TravelMode
    , grantContribution : Int
    , sponsorship : Maybe String
    }


type alias CouplePurchaseData =
    { attendee1Name : Evergreen.V56.Name.Name
    , attendee2Name : Evergreen.V56.Name.Name
    , billingEmail : Evergreen.V56.EmailAddress.EmailAddress
    , country : String.Nonempty.NonemptyString
    , originCity : String.Nonempty.NonemptyString
    , primaryModeOfTravel : Evergreen.V56.TravelMode.TravelMode
    , grantContribution : Int
    , sponsorship : Maybe String
    }


type PurchaseFormValidated
    = CampfireTicketPurchase SinglePurchaseData
    | CampTicketPurchase SinglePurchaseData
    | CouplesCampTicketPurchase CouplePurchaseData
