module Page.Plotting exposing (Model, Msg, init, randomMatrix, subscriptions, toSession, update, view)

import Array exposing (Array)
import Basics as Math
import Html exposing (..)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import List.Extra
import PolylinearScale exposing (polylinearScale)
import Random exposing (generate, int)
import Random.Array
import Round
import Session exposing (Session)
import Svg exposing (..)
import Svg.Attributes exposing (cx, cy, d, dy, fill, height, pointerEvents, r, stroke, strokeWidth, textAnchor, viewBox, width, x, y)
import Task



-- MODEL


type alias Model =
    { session : Session
    , numSquares : Int
    , grid : Grid
    }


init : Session -> ( Model, Cmd Msg )
init session =
    ( { session = session
      , numSquares = 10
      , grid = Array.initialize 10 (\_ -> Array.repeat 10 1)
      }
    , Task.succeed GenerateNewGrid |> Task.perform identity
    )



-- VIEW


type alias ChartDimensions =
    { width : Int
    , height : Int
    , numSquares : Int
    }


chartDimensions =
    { width = 500, height = 500, numSquares = 10 }


scale =
    polylinearScale [ ( 0, 500 ), ( 500, 0 ) ]


value =
    let
        scaledValue =
            scale 0

        safeValue =
            case scaledValue of
                Nothing ->
                    ""

                Just v ->
                    String.fromFloat v
    in
    safeValue


type Line
    = VerticalBlackLine
    | HorizontalYellowLine
    | DiagonalRightRedLine
    | DiagonalLeftBlueLine
    | NoLine


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


viewSquare : Point -> Float -> Int -> List (Svg Msg)
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
                    svgLineView [ rectDimensions.centerBottom, rectDimensions.centerTop ] [ stroke "black" ]

                HorizontalYellowLine ->
                    svgLineView [ rectDimensions.centerLeft, rectDimensions.centerRight ] [ stroke "yellow" ]

                DiagonalRightRedLine ->
                    svgLineView [ rectDimensions.bottomLeft, rectDimensions.topRight ] [ stroke "red" ]

                DiagonalLeftBlueLine ->
                    svgLineView [ rectDimensions.bottomRight, rectDimensions.topLeft ] [ stroke "blue" ]

                NoLine ->
                    svgLineView [] []
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


view : Model -> { title : String, content : Html Msg }
view model =
    { title = "Conduit"
    , content =
        div [ class "home-page" ]
            [ div [ class "container page" ]
                [ div [ class "row" ]
                    [ div [ class "col-md-9" ] <|
                        [ viewPiece chartDimensions model.grid
                        , button [ onClick GenerateNewGrid ]
                            [ Html.text "Regenerate" ]
                        , gridToTable model.grid
                        ]
                    ]
                ]
            ]
    }



-- DEBUGGING


viewPiece : { width : Float, height : Float, numSquares : Int } -> Grid -> Html Msg
viewPiece dimensions grid =
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
        [ width <| String.fromInt chartDimensions.width
        , height <| String.fromInt chartDimensions.height
        , viewBox <| "0 0 " ++ String.fromInt chartDimensions.width ++ " " ++ String.fromInt chartDimensions.height
        ]
    <|
        -- Flattening
        List.foldr (++) []
        <|
            List.map
                (\{ x, y, gridValue } -> viewSquare { x = x, y = y } squareHeight gridValue)
                squarePoints


gridToTable : Grid -> Html Msg
gridToTable grid =
    let
        columnToTd : Array Int -> List (Html Msg)
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


arrayToString array =
    Array.foldr (\current acc -> String.fromInt current ++ acc) "" array


type alias Point =
    { x : Float
    , y : Float
    }


type alias Points =
    List Point


roundPoint val =
    Round.round 2 (val * 100 / 100)


svgLineView : Points -> List (Html.Attribute Msg) -> Html Msg
svgLineView points attributes =
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


cartesian : List a -> List b -> List ( a, b )
cartesian xs ys =
    List.concatMap (\x -> List.map (\y -> ( x, y )) ys) xs



-- VISUALIZATION RULES
-- UPDATE


type alias Grid =
    Array (Array Int)


type Msg
    = GotSession Session
    | GenerateNewGrid
    | NewGrid Grid


randomMatrix =
    Random.Array.array 10 (Random.Array.array 10 (int 0 500))


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GenerateNewGrid ->
            ( model, generate NewGrid randomMatrix )

        NewGrid grid ->
            ( { model | grid = grid }, Cmd.none )

        GotSession session ->
            ( model, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Session.changes GotSession (Session.navKey model.session)



-- EXPORT


toSession : Model -> Session
toSession model =
    model.session
