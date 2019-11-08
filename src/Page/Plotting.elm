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
    , grid : SolLewitt.Grid
    }



-- MSG


type Msg
    = GenerateNewGrid Int
    | NewGrid SolLewitt.Grid
    | IncrementedNumSquares
    | DecrementedNumSquares


init : Session -> ( Model, Cmd Msg )
init session =
    ( { session = session
      , numSquares = 10
      , grid = Array.initialize 10 (\_ -> Array.repeat 10 1)
      }
    , Task.succeed (GenerateNewGrid 10) |> Task.perform identity
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
                        { width = 500, height = 500, numSquares = model.numSquares }
                        model.grid

                    --                , gridToTable model.grid
                    ]
                ]
            , div [ class "self-end flex flex-col h-screen w-1/6 p-5 bg-gray-200 border border-gray-400" ]
                [ blueButtonView
                    [ onClick <| GenerateNewGrid model.numSquares ]
                    "Create new grid"
                , text "Number of squares"
                , div [ class "flex flex-row mt-5" ]
                    [ redButtonView [ onClick DecrementedNumSquares ] "-"
                    , div [ class "self-center" ] [ text <| String.fromInt model.numSquares ]
                    , greenButtonView [ onClick IncrementedNumSquares ] "+"
                    ]
                ]
            ]
    }


blueButtonView : List (Attribute msg) -> String -> Html msg
blueButtonView attributes internalText =
    button
        (class "bg-transparent hover:bg-blue-500 text-blue-700 font-semibold hover:text-white py-2 px-4 border border-blue-500 hover:border-transparent rounded" :: attributes)
        [ Html.text internalText ]


redButtonView : List (Attribute msg) -> String -> Html msg
redButtonView attributes internalText =
    button
        (class "bg-transparent hover:bg-red-500 text-red-700 font-semibold hover:text-white py-2 px-4 border border-red-500 hover:border-transparent rounded" :: attributes)
        [ Html.text internalText ]


greenButtonView : List (Attribute msg) -> String -> Html msg
greenButtonView attributes internalText =
    button
        (class "bg-transparent hover:bg-green-500 text-green-700 font-semibold hover:text-white py-2 px-4 border border-green-500 hover:border-transparent rounded" :: attributes)
        [ Html.text internalText ]



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GenerateNewGrid numSquares ->
            ( model, generate NewGrid <| randomMatrix numSquares )

        NewGrid grid ->
            ( { model | grid = grid }, Cmd.none )

        IncrementedNumSquares ->
            let
                newNumSquares =
                    model.numSquares + 1
            in
            ( { model | numSquares = newNumSquares }, generate NewGrid <| randomMatrix newNumSquares )

        DecrementedNumSquares ->
            let
                newNumSquares =
                    model.numSquares - 1
            in
            ( { model | numSquares = newNumSquares }, generate NewGrid <| randomMatrix newNumSquares )



-- EXPORT


toSession : Model -> Session
toSession model =
    model.session



-- EFFECTS


randomMatrix numSquares =
    Random.Array.array numSquares (Random.Array.array numSquares (int 0 500))
