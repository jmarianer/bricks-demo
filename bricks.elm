import Html exposing (..)
import Html.Attributes exposing (style)

main = Html.beginnerProgram { model = model, view = view, update = update }

-- MODEL
type alias Model = {
  width: Int,
  height: Int,
  depth: Int,
  blocks: List Block
}
type Orientation = X|Y|Z
type alias Block = {
  length: Int,
  x: Int,
  y: Int,
  orientation: Orientation
}

model : Model
model = { width = 4, height = 2, depth = 3, blocks = [] }

-- UPDATE
type Msg = Int
update : Msg -> Model -> Model
update msg model = model

-- VIEW
view : Model -> Html Msg
view model =
  div [style [("display", "flex"), ("align-items", "center"), ("width", "100%"), ("height", "100%"), ("justify-content", "center")]] [
    div [style [("transform", "rotateX(76deg) rotateY(16deg) rotateZ(30deg)"), ("transform-style", "preserve-3d"), ("transform-origin", "bottom left"), ("box-sizing", "border-box")]] (
      box 0 0 0 model.width model.height model.depth []
      ++ box 1 1 3 2 3 6 [("background-color", "blue"), ("opacity", "0.5")]
  )]

standardStyles = [("transform-origin", "top left"), ("border", "1px solid black"), ("position", "absolute"), ("box-sizing", "border-box")]

pixelsPerBlock = 50
toPixels x = toString (pixelsPerBlock * x) ++ "px"

type alias Style = (String, String)
width : Int -> Style
width x = ("width", toPixels x)

height : Int -> Style
height y = ("height", toPixels y)

--transform : Orientation -> Int -> Style
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
    div [style ([width (y2 - y1), height (x2 - x1), transform Z x1 y1 z1] ++ otherStyles)] [],
    div [style ([width (y2 - y1), height (x2 - x1), transform Z x1 y1 z2] ++ otherStyles)] [],
    div [style ([width (z2 - z1), height (x2 - x1), transform Y z1 x1 y1] ++ otherStyles)] [],
    div [style ([width (z2 - z1), height (x2 - x1), transform Y z1 x1 y2] ++ otherStyles)] [],
    div [style ([width (y2 - y1), height (z2 - z1), transform X y1 z1 x1] ++ otherStyles)] [],
    div [style ([width (y2 - y1), height (z2 - z1), transform X y1 z1 x2] ++ otherStyles)] []
  ]
