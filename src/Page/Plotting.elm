module Page.Plotting exposing (Model, Msg, init, randomMatrix, toSession, update, view)

import Array exposing (Array)
import Html exposing (..)
import Html.Attributes exposing (attribute, class)
import Html.Events exposing (onClick, onInput)
import Random exposing (generate, int)
import Random.Array
import Session exposing (Session)
import SolLewitt
import String exposing (toInt)
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
    | GotNumSquaresInput Int
    | NoOp


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
            , visualizationEditorView model.numSquares
            ]
    }


blueButtonView : List (Attribute msg) -> String -> Html msg
blueButtonView attributes internalText =
    button
        (class "bg-blue-500 hover:bg-blue-700 text-white font-bold py-2 px-4 border border-blue-700 rounded" :: attributes)
        [ Html.text internalText ]


visualizationEditorView : Int -> Html Msg
visualizationEditorView numSquares =
    let
        handleNumSquareOnInput : String -> Msg
        handleNumSquareOnInput inputValue =
            case toInt inputValue of
                Just newNumSquares ->
                    GotNumSquaresInput newNumSquares

                Nothing ->
                    NoOp
    in
    div [ class "self-end flex flex-col h-screen w-1/6 p-5 bg-gray-200 border border-gray-400" ]
        [ blueButtonView
            [ onClick <| GenerateNewGrid numSquares ]
            "Create new grid"
        , div [ class "mt-5" ] [ text "Number of squares" ]

        -- TODO make a form, and figure out how to disable form submit in elm, sure it's just the usual attribute
        , div [ class "flex mt-2" ]
            [ button
                [ class "bg-gray-300 hover:bg-gray-400 text-gray-800 font-bold py-2 px-4 rounded-l"
                , onClick DecrementedNumSquares
                ]
                [ text "-" ]
            , input
                [ class "flex-grow w-1/3 p-2 text-center"
                , attribute "type" "number"
                , Html.Attributes.type_ "number"
                , Html.Attributes.value <| String.fromInt numSquares
                , onInput handleNumSquareOnInput
                ]
                [ text <| String.fromInt numSquares ]
            , button
                [ class "flex-grow-none bg-gray-300 hover:bg-gray-400 text-gray-800 font-bold py-2 px-4 rounded-r"
                , onClick IncrementedNumSquares
                ]
                [ text "+" ]
            ]
        ]



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

        GotNumSquaresInput newNumSquares ->
            ( { model | numSquares = newNumSquares }, generate NewGrid <| randomMatrix newNumSquares )

        NoOp ->
            ( model, Cmd.none )



-- EXPORT


toSession : Model -> Session
toSession model =
    model.session



-- EFFECTS


randomMatrix numSquares =
    Random.Array.array numSquares (Random.Array.array numSquares (int 0 500))
