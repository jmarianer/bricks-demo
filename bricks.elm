import Html exposing (..)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)
import Board exposing (Board, Orientation(..))

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
blockToBox : Board.Block -> List (String, String) -> List (Html msg)
blockToBox { length, x, y, z, orientation } =
  case orientation of
    X -> box x y z (x+length) (y+1) (z+1)
    Y -> box x y z (x+1) (y+length) (z+1)
    Z -> box x y z (x+1) (y+1) (z+length)

view : Board -> Html Msg
view model =
  div [style [("display", "flex"), ("align-items", "center"), ("width", "100%"), ("height", "100%"), ("justify-content", "center")]] [
    div [style [("transform", "rotateX(76deg) rotateY(187deg) rotateZ(320deg)"), ("transform-style", "preserve-3d"), ("transform-origin", "bottom left"), ("box-sizing", "border-box")]] (
      box 0 0 0 model.width model.depth model.height []
      ++ blockToBox model.mainBlock [("background-color", "red")]
      ++ List.concatMap (\block -> blockToBox block [("background-color", "blue"), ("opacity", "0.5")]) model.blocks
    ),
    button [onClick 0] [text "0"],
    button [onClick 1] [text "1"],
    button [onClick 2] [text "2"]
  ]

standardStyles = [("transform-origin", "top left"), ("border", "1px solid black"), ("position", "absolute"), ("box-sizing", "border-box"), ("transition", "all ease-in-out .3s")]

pixelsPerBlock = 50
toPixels x = toString (pixelsPerBlock * x) ++ "px"

type alias Style = (String, String)
width : Int -> Style
width x = ("width", toPixels x)

height : Int -> Style
height y = ("height", toPixels y)

transform : Orientation -> Int -> Int -> Int -> Style
transform o x y z = 
  let
    translate x y z = String.concat <| List.concat <| List.map2 (\d x -> ["translate", d, "(", toPixels x, ")"]) ["X", "Y", "Z"] [x, y, z]
    transform = case o of
      X -> "rotateX(-90deg)" ++ translate x y z
      Y -> "rotateY(90deg)" ++ translate x y z
      Z -> translate x y -z
  in
    ("transform", transform)

box x1 y1 z1 x2 y2 z2 styles = 
  let
    otherStyles = styles ++ standardStyles
  in
  [
    div [style ([width (y2 - y1), height (x2 - x1), transform Z y1 x1 z1] ++ otherStyles)] [],
    div [style ([width (y2 - y1), height (x2 - x1), transform Z y1 x1 z2] ++ otherStyles)] [],
    div [style ([width (z2 - z1), height (x2 - x1), transform Y z1 x1 y1] ++ otherStyles)] [],
    div [style ([width (z2 - z1), height (x2 - x1), transform Y z1 x1 y2] ++ otherStyles)] [],
    div [style ([width (y2 - y1), height (z2 - z1), transform X y1 z1 x1] ++ otherStyles)] [],
    div [style ([width (y2 - y1), height (z2 - z1), transform X y1 z1 x2] ++ otherStyles)] []
  ]
