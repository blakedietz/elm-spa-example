module Page.Plotting exposing (Model, Msg, init, randomMatrix, toSession, update, view)

import Array exposing (Array)
import Html exposing (..)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import Random exposing (generate, int)
import Random.Array
import Session exposing (Session)
import SolLewitt
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


view : Model -> { title : String, content : Html Msg }
view model =
    { title = "Conduit"
    , content =
        div [ class "flex flex-row justify-between" ]
            [ div [ class "flex-grow mt-5" ]
                [ div [ class "flex flex-col justify-center items-center" ]
                    [ SolLewitt.view
                        chartDimensions
                        model.grid

                    --                , gridToTable model.grid
                    ]
                ]
            , div [ class "self-end flex flex-col h-screen w-1/6 p-5 bg-gray-200 border border-gray-400" ]
                [ button
                    [ class "bg-transparent hover:bg-blue-500 text-blue-700 font-semibold hover:text-white py-2 px-4 border border-blue-500 hover:border-transparent rounded"
                    , onClick GenerateNewGrid
                    ]
                    [ Html.text "Create a new grid" ]
                ]
            ]
    }



-- VISUALIZATION RULES
-- UPDATE


type alias Grid =
    Array (Array Int)


type Msg
    = GenerateNewGrid
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



-- EXPORT


toSession : Model -> Session
toSession model =
    model.session
