module Pages.GoogleOAuthRedirect exposing (..)

import Element exposing (..)
import Element.Font
import Types exposing (..)
import View.Button


view : LoadedModel -> Element FrontendMsg
view model =
    Element.column
        [ width fill
        , height fill
        ]
        [ el [ centerX, centerY, Element.Font.size 40 ] <| Element.text "Loading..." ]
