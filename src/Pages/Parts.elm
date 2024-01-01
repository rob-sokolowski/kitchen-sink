module Pages.Parts exposing (footer, generic, header)

import Element exposing (Element)
import Element.Background
import Element.Font
import Route exposing (Route(..))
import Theme
import Types
import View.Color


generic : Types.LoadedModel -> (Types.LoadedModel -> Element msg) -> Element msg
generic model view =
    Element.column
        [ Element.width Element.fill, Element.height Element.fill ]
        [ header model.route { window = model.window, isCompact = True }
        , Element.column
            (Element.padding 20
                :: Element.scrollbarY
                :: Element.height (Element.px <| model.window.height - 95)
                :: Theme.contentAttributes
            )
            [ view model
            ]
        , footer model.route model
        ]


header : Route -> { window : { width : Int, height : Int }, isCompact : Bool } -> Element msg
header route config =
    Element.el
        [ Element.Background.color View.Color.blue
        , Element.paddingXY 24 16
        , Element.width (Element.px config.window.width)
        , Element.alignTop
        ]
        (Element.wrappedRow
            ([ Element.spacing 32
             , Element.Background.color View.Color.blue
             , Element.Font.color (Element.rgb 1 1 1)
             ]
                ++ Theme.contentAttributes
            )
            [ Element.link
                [ tabColor route HomepageRoute ]
                { url = Route.encode HomepageRoute, label = Element.text "Lamdera Kitchen Sink" }
            , Element.link
                [ tabColor route Features
                ]
                { url = Route.encode Features, label = Element.text "Features" }
            , Element.link
                [ tabColor route Notes ]
                { url = Route.encode Notes, label = Element.text "Notes" }
            , Element.link
                [ tabColor route Purchase ]
                { url = Route.encode Purchase, label = Element.text "Purchase" }
            ]
        )


tabColor currentRoute route =
    if currentRoute == route then
        Element.Font.color View.Color.orange

    else
        Element.Font.color View.Color.white


footer : Route -> Types.LoadedModel -> Element msg
footer route model =
    Element.el
        [ Element.Background.color View.Color.blue
        , Element.paddingXY 24 16
        , Element.width Element.fill
        , Element.alignBottom
        ]
        (Element.wrappedRow
            ([ Element.spacing 32
             , Element.Background.color View.Color.blue
             , Element.Font.color (Element.rgb 1 1 1)
             ]
                ++ Theme.contentAttributes
            )
            [ Element.link
                [ tabColor route Brillig ]
                { url = Route.encode Brillig, label = Element.text "Brillig" }
            , Element.el [ Element.Background.color View.Color.black, Element.Font.color View.Color.white ] (Element.text model.message)
            ]
        )
