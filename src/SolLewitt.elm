module SolLewitt exposing (..)

import Array exposing (Array)
import Basics as Math
import Html exposing (..)
import Plotting exposing (cartesian)
import PolylinearScale exposing (polylinearScale)
import Svg exposing (..)
import Svg.Attributes exposing (fill, height, stroke, viewBox, width, x, y)



-- TYPES


type alias Grid =
    Array (Array Int)


type Line
    = VerticalBlackLine
    | HorizontalYellowLine
    | DiagonalRightRedLine
    | DiagonalLeftBlueLine
    | NoLine



-- VIEW


view : { width : Float, height : Float, numSquares : Int } -> Grid -> Html msg
view dimensions grid =
    let
        xScale =
            polylinearScale [ ( 0.0, 0.0 ), ( toFloat dimensions.numSquares, dimensions.width ) ]

        yScale =
            polylinearScale [ ( 0.0, 0.0 ), ( toFloat dimensions.numSquares, dimensions.height ) ]

        pointValues maybePoints =
            maybePoints
                |> List.foldr
                    (\{ x, y, gridValue } acc ->
                        case ( x, y, gridValue ) of
                            ( Just x1, Just y1, Just gridVal ) ->
                                { x = x1, y = y1, gridValue = gridVal } :: acc

                            ( _, _, _ ) ->
                                acc
                    )
                    []

        squarePoints : List { x : Float, y : Float, gridValue : Int }
        squarePoints =
            cartesian (List.range 0 dimensions.numSquares) (List.range 0 dimensions.numSquares)
                |> List.map (\( x, y ) -> { x = xScale <| toFloat x, y = yScale <| toFloat y, gridValue = Array.get y grid |> Maybe.andThen (Array.get x) })
                |> pointValues

        squareHeight =
            dimensions.height / toFloat dimensions.numSquares

        squareWidth =
            dimensions.width / toFloat dimensions.numSquares
    in
    svg
        [ Svg.Attributes.id "sol_lewitt"
        , viewBox <| "0 0 " ++ String.fromFloat dimensions.width ++ " " ++ String.fromFloat dimensions.height
        ]
    <|
        -- Flattening
        List.foldr (++) []
        <|
            List.map
                (\{ x, y, gridValue } -> viewSquare { x = x, y = y } squareHeight gridValue)
                squarePoints


viewSquare : Plotting.Point -> Float -> Int -> List (Svg msg)
viewSquare point size line =
    let
        rectDimensions =
            { topLeft = { x = point.x, y = point.y }
            , topRight = { x = point.x + size, y = point.y }
            , bottomLeft = { x = point.x, y = point.y - size }
            , bottomRight = { x = point.x + size, y = point.y - size }
            , centerLeft = { x = point.x, y = point.y + size / 2 }
            , centerRight = { x = point.x + size, y = point.y + size / 2 }
            , centerBottom = { x = point.x + size / 2, y = point.y + size }
            , centerTop = { x = point.x + size / 2, y = point.y }
            , center = { x = point.x + size / 2, y = point.y + size / 2 }
            }

        lines lineEnum =
            case lineEnum of
                VerticalBlackLine ->
                    Plotting.line [ rectDimensions.centerBottom, rectDimensions.centerTop ] [ stroke "black" ]

                HorizontalYellowLine ->
                    Plotting.line [ rectDimensions.centerLeft, rectDimensions.centerRight ] [ stroke "yellow" ]

                DiagonalRightRedLine ->
                    Plotting.line [ rectDimensions.bottomLeft, rectDimensions.topRight ] [ stroke "red" ]

                DiagonalLeftBlueLine ->
                    Plotting.line [ rectDimensions.bottomRight, rectDimensions.topLeft ] [ stroke "blue" ]

                NoLine ->
                    Plotting.line [] []
    in
    [ rect
        [ x <| String.fromFloat point.x
        , y <| String.fromFloat point.y
        , width <| String.fromFloat size
        , height <| String.fromFloat size
        , fill "none"
        , stroke "black"
        ]
        []
    , lines (blackLine line)
    , lines (blueLine line)
    , lines (redLine line)
    , lines (yellowLine line)

    --    , Svg.text_
    --        [ pointerEvents "none"
    --        , x <| String.fromFloat rectDimensions.center.x
    --        , y <| String.fromFloat rectDimensions.center.y
    --        , textAnchor "middle"
    --        ]
    --        [ Svg.tspan [] [ Svg.text <| String.fromInt line ] ]
    ]



-- TODO: Make sure to add view previx or suffix


yellowLine num =
    if Math.modBy 2 num == 0 then
        HorizontalYellowLine

    else
        NoLine


blackLine num =
    if Math.modBy 3 num == 0 then
        VerticalBlackLine

    else
        NoLine


blueLine num =
    if Math.modBy 4 num == 0 then
        DiagonalLeftBlueLine

    else
        NoLine


redLine num =
    if Math.modBy 5 num == 0 then
        DiagonalRightRedLine

    else
        NoLine



-- DEBUGGING


gridToTable : Grid -> Html msg
gridToTable grid =
    let
        columnToTd : Array Int -> List (Html msg)
        columnToTd column =
            column
                |> Array.map (\entry -> td [] [ Html.text <| String.fromInt entry ])
                |> Array.toList

        rows =
            Array.toList <| Array.map (\row -> tr [] <| columnToTd row) grid
    in
    table []
        [ tbody []
            rows
        ]


gridToString : Grid -> String
gridToString grid =
    let
        stringifiedRows =
            Array.map (\row -> arrayToString row) grid
    in
    Array.foldr (\current acc -> acc ++ current ++ "\n") "" stringifiedRows



-- INTERNAL


arrayToString array =
    Array.foldr (\current acc -> String.fromInt current ++ acc) "" array
