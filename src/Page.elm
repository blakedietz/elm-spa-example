module Page exposing (Page(..), view)

import Browser exposing (Document)
import Html exposing (..)
import Html.Attributes exposing (class, href)


{-| Determines which navbar link (if any) will be rendered as active.

Note that we don't enumerate every page here, because the navbar doesn't
have links for every page. Anything that's not part of the navbar falls
under Other.

-}
type Page
    = Other
    | SolLewitt


{-| Take a page's Html and frames it with a header and footer.

The caller provides the current user, so we can display in either
"signed in" (rendering username) or "signed out" mode.

isLoading is for determining whether we should show a loading spinner
in the header. (This comes up during slow page transitions.)

-}
view : Page -> { title : String, content : Html msg } -> Document msg
view page { title, content } =
    { title = title ++ " - plott.id"
    , body = viewHeader :: [ content ]
    }


viewHeader : Html msg
viewHeader =
    nav [ class "flex items-center justify-between flex-wrap bg-teal-500 p-6" ]
        [ div [ class "flex items-center flex-shrink-0 text-white mr-6" ]
            [ span [ class "font-semibold text-xl tracking-tight" ]
                [ text "plott.id" ]
            ]

        --            TODO: Get rid of
        , div [ class "w-full block flex-grow flex items-center w-auto" ]
            [ div [ class "text-sm lg:flex-grow" ]
                [ a [ class "block mt-4 inline-block mt-0 text-teal-200 hover:text-white mr-4", href "#responsive-header" ]
                    [ text "Docs      " ]
                , a [ class "block mt-4 inline-block mt-0 text-teal-200 hover:text-white mr-4", href "#responsive-header" ]
                    [ text "Examples      " ]
                , a [ class "block mt-4 inline-block mt-0 text-teal-200 hover:text-white", href "#responsive-header" ]
                    [ text "Blog      " ]
                ]
            ]
        ]



-- TODO: Add this or remove it.


viewFooter : Html msg
viewFooter =
    footer []
        [ div [ class "container" ]
            [ a [ class "logo-font", href "/" ] [ text "conduit" ]
            , span [ class "attribution" ]
                [ text "An interactive learning project from "
                , a [ href "https://thinkster.io" ] [ text "Thinkster" ]
                , text ". Code & design licensed under MIT."
                ]
            ]
        ]
