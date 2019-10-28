module Page.Plotting exposing (Model, Msg, init, subscriptions, toSession, update, view)

import Array exposing (Array)
import Html exposing (..)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import List.Extra
import Maybe.Extra as Maybe
import Random exposing (generate, int)
import Random.Array
import Round
import Session exposing (Session)
import Svg exposing (..)
import Svg.Attributes exposing (d, height, stroke, viewBox, width, x, y)



-- MODEL


type alias Model =
    { session : Session
    , grid : Grid
    }


init : Session -> ( Model, Cmd Msg )
init session =
    ( { session = session
      , grid = Array.fromList [ 1, 2, 3, 4, 5 ]
      }
    , Cmd.batch
        []
    )



-- VIEW


view : Model -> { title : String, content : Html Msg }
view model =
    { title = "Conduit"
    , content =
        div [ class "home-page" ]
            [ div [ class "container page" ]
                [ div [ class "row" ]
                    [ div [ class "col-md-9" ] <|
                        [ svg
                            [ width "120", height "120", viewBox "0 0 120 120" ]
                            [ svgLine [ { x = 0, y = 0 }, { x = 100, y = 100 } ]
                            ]
                        , button [ onClick GenerateNewGrid ]
                            []
                        , Html.text <| Array.foldr (\current acc -> String.fromInt current ++ acc) "" model.grid
                        ]
                    ]
                ]
            ]
    }


type alias Point =
    { x : Float
    , y : Float
    }


type alias Points =
    List Point


roundPoint val =
    Round.round 2 (val * 100 / 100)


svgLine : Points -> Html Msg
svgLine points =
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
    path [ d lineString, stroke "red" ] []



-- VISUALIZATION RULES
-- UPDATE


type alias Grid =
    --    Array (Array Int)
    Array Int


type Msg
    = GotSession Session
    | GenerateNewGrid
    | NewGrid Grid


randomMatrix =
    --    Random.Array.array 5 (Random.Array.array 5 (int 0 4))
    Random.Array.array 5 (int 0 4)


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
