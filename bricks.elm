import Board exposing (Board, Orientation(..))
import Html exposing (..)
import Html.Attributes exposing (style, type_)
import Html.Events exposing (onClick, onInput)
import Board exposing (Board, Orientation(..))
import Time exposing (Time, second)
import ShowBoard
import SolveBoard

-- MODEL
type alias Model = {
  input : String,
  steps : List Board,
  enabled : Bool
  }

initialModel : Model
initialModel = { input = "", steps = [], enabled = False }

-- UPDATE
type Msg = Tick Time | Start | Show | Input String
update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  let
    nextStep = case List.tail model.steps of
      Just [] -> { model | enabled = False }
      Just s  -> { model | steps = s }
      Nothing -> { model | enabled = False }

    newModel = case msg of
      Start   -> { model | steps = SolveBoard.solveBoard <| Board.toBoard model.input, enabled = True }
      Tick _  -> nextStep
      Show    -> { model | steps = [Board.toBoard model.input], enabled = False }
      Input s -> { model | input = s }
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
  let
    maybeShowBoard =
      case List.head model.steps of
        Nothing -> []
        Just x  -> [ShowBoard.toHtml x]
  in
    div [mainDivStyle] [
      div [secondaryDivStyle] maybeShowBoard,
      input [ type_ "text", onInput Input ] [],
      button [onClick Show] [text "Show"],
      button [onClick Start] [text "Solve"]
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
