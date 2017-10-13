import Board exposing (Board, Orientation(..))
import Html exposing (..)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)
import Board exposing (Board, Orientation(..))
import Time exposing (Time, second)
import ShowBoard
import SolveBoard

-- Utilities
fromJust : Maybe a -> a
fromJust x = case x of
    Just y -> y
    Nothing -> Debug.crash "error: fromJust Nothing"

-- Steps to the solution
testBoard = Board.toBoard "334;2112Y;2010Y;2200Y;2101Z"

steps = SolveBoard.solveBoard testBoard

-- MODEL
type alias Model = {
  steps : List Board,
  enabled : Bool
  }

initialModel : Model
initialModel = { steps = steps, enabled = False }

-- UPDATE
type Msg = Tick Time | Start | Reset
update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  let
    nextStep = case List.tail model.steps of
      Just s  -> { model | steps = s }
      Nothing -> model

    newModel = case msg of
      Start  -> { nextStep | enabled = True }
      Tick _ -> nextStep
      Reset  -> { model | steps = steps, enabled = False }
  in
    (newModel, Cmd.none)


-- VIEW
-- TODO either learn elm-css or use style-elements
mainDivStyle = style [
  ("display", "flex"),
  ("flex-direction", "column"),
  ("align-items", "center"),
  ("width", "100%"),
  ("height", "100%"),
  ("justify-content", "space-around")
  ]

secondaryDivStyle = style [
  ("border", " 1px solid black"),
  ("width", " 500px"),
  ("height", " 500px"),
  ("display", " flex"),
  ("justify-content", " center"),
  ("align-items", " center"),
  ("overflow", "none")
  ]

view : Model -> Html Msg
view model =
  div [mainDivStyle] [
    div [secondaryDivStyle] [ShowBoard.toHtml <| fromJust <| List.head model.steps],
    button [onClick Start] [text "Solve"],
    button [onClick Reset] [text "Reset"]
  ]


-- MAIN
subscriptions : Model -> Sub Msg
subscriptions {enabled} =
  if enabled then Time.every second Tick else Sub.none

main =
  Html.program {
    init = (initialModel, Cmd.none),
    view = view,
    update = update,
    subscriptions = subscriptions
  }
