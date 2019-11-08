module Plotting exposing (Point, Points, line)

import Html exposing (..)
import List.Extra
import Round
import Svg exposing (path)
import Svg.Attributes exposing (d)


type alias Point =
    { x : Float
    , y : Float
    }


type alias Points =
    List Point


line : Points -> List (Html.Attribute msg) -> Html msg
line points attributes =
    let
        reducer : Int -> Point -> String -> String
        reducer index { x, y } acc =
            if index == 0 then
                acc ++ "M " ++ roundPoint x ++ "," ++ roundPoint y

            else
                acc ++ "L " ++ roundPoint x ++ "," ++ roundPoint y

        lineString : String
        lineString =
            List.Extra.indexedFoldl reducer "" points
    in
    path (attributes ++ [ d lineString ]) []



-- INTERNAL


roundPoint val =
    Round.round 2 (val * 100 / 100)
