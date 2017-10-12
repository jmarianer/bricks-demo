import Board exposing (Board, Orientation(..))
import Html exposing (..)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)
import ShowBoard

main = Html.beginnerProgram { model = model, view = view, update = update }

-- MODEL
myTest =  "334;2112Y;2010Y;2200Y;2101Z"
myTest1 = "334;2112Y;2010Y;2200Y;2100Z"
myTest2 = "334;2102Y;2010Y;2200Y;2100Z"

model : Board
model = Board.toBoard myTest

-- UPDATE
type alias Msg = Int
update : Msg -> Board -> Board
update msg model = case msg of
  0 -> Board.toBoard myTest
  1 -> Board.toBoard myTest1
  2 -> Board.toBoard myTest2
  _ -> model

-- VIEW
view : Board -> Html Msg
view model =
  div [style [("display", "flex"), ("align-items", "center"), ("width", "100%"), ("height", "100%"), ("justify-content", "center")]] [
    ShowBoard.toHtml model,
    button [onClick 0] [text "0"],
    button [onClick 1] [text "1"],
    button [onClick 2] [text "2"]
  ]
