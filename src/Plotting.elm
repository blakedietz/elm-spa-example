module Plotting exposing (Point, Points, cartesian, line)

import Html exposing (..)
import List.Extra
import Round
import Svg exposing (Svg, path)
import Svg.Attributes exposing (d)


type alias Point =
    { x : Float
    , y : Float
    }


type alias Points =
    List Point


line : Points -> List (Html.Attribute msg) -> Svg msg
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



-- EXTERNAL HELPER


cartesian : List a -> List b -> List ( a, b )
cartesian xs ys =
    List.concatMap (\x -> List.map (\y -> ( x, y )) ys) xs



-- INTERNAL HELPER


roundPoint val =
    Round.round 2 (val * 100 / 100)
