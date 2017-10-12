import Board exposing (Board, Orientation(..))
import Html exposing (..)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)
import Board exposing (Board, Orientation(..))
import Time exposing (Time, second)
import ShowBoard

-- Utilities
fromJust : Maybe a -> a
fromJust x = case x of
    Just y -> y
    Nothing -> Debug.crash "error: fromJust Nothing"


-- Hardcoded steps to the solution
myTest =  "334;2112Y;2010Y;2200Y;2101Z"
myTest1 = "334;2112Y;2010Y;2200Y;2100Z"
myTest2 = "334;2102Y;2010Y;2200Y;2100Z"

steps = ["334;2112Y;2010Y;2200Y;2101Z",
         "334;2112Y;2010Y;2200Y;2100Z",
         "334;2102Y;2010Y;2200Y;2100Z"]

main =
  Html.program {
    init = (Board.toBoard <| fromJust <| List.head steps, Cmd.none),
    view = view,
    update = update,
    subscriptions = \_ -> Time.every second Tick
  }


-- UPDATE
type Msg = Tick Time
update : Msg -> Board -> (Board, Cmd Msg)
update (Tick i) model =
  let
    newModel = case i of
      0 -> Board.toBoard myTest
      1 -> Board.toBoard myTest1
      2 -> Board.toBoard myTest2
      _ -> model
  in
    (newModel, Cmd.none)


-- VIEW
view : Board -> Html Msg
view model =
  div [style [("display", "flex"), ("align-items", "center"), ("width", "100%"), ("height", "100%"), ("justify-content", "center")]] [
    ShowBoard.toHtml model,
    button [onClick <| Tick 0] [text "0"],
    button [onClick <| Tick 1] [text "1"],
    button [onClick <| Tick 2] [text "2"]
  ]
