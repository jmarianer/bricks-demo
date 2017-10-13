module ShowBoard exposing (toHtml)

import Board exposing (Board, Block, Orientation(..))
import Html exposing (Html, div)
import Html.Attributes

-- Hardcoded styles. TODO: Learn elm-css
type alias StyleElement = (String, String)
type alias Style = List StyleElement

boxStyle : Style
boxStyle = [
  ("transform-origin", "top left"),
  ("border", "1px solid black"),
  ("position", "absolute"),
  ("box-sizing", "border-box"),
  ("transition", "all ease-in-out .3s")]

mainBlockStyle : Style
mainBlockStyle = boxStyle ++ [("background-color", "red")]

otherBlockStyle : Style
otherBlockStyle = boxStyle ++ [("background-color", "blue"), ("opacity", "0.5")]


-- Styling utilities
pixelsPerBlock = 50
toPixels x = toString (pixelsPerBlock * x) ++ "px"

width : Int -> StyleElement
width x = ("width", toPixels x)

height : Int -> StyleElement
height y = ("height", toPixels y)

transform : Orientation -> Int -> Int -> Int -> StyleElement
transform orientation x y z = 
  let
    translate x y z = String.concat <| List.concat <| List.map2 (\d x -> ["translate", d, "(", toPixels x, ")"]) ["X", "Y", "Z"] [x, y, z]
    transform = case orientation of
      X -> "rotateX(-90deg)" ++ translate x y z
      Y -> "rotateY(90deg)" ++ translate x y z
      Z -> translate x y -z
  in
    ("transform", transform)


-- Functions
toHtml : Board -> Html msg
toHtml board =
  let
    mainDivStyle = [
      width board.width,
      height board.depth,
      ("transform", "rotateX(76deg) rotateY(187deg) rotateZ(320deg) translateZ(" ++ (toPixels <| (toFloat board.height)/2) ++ ")"),
      ("transform-style", "preserve-3d"),
      ("box-sizing", "border-box")]
    boundingBox = box 0 0 0 board.width board.depth board.height boxStyle

    boardMainBlock = board.mainBlock
    winPosition = -boardMainBlock.length
    mungeMainBlock : (Bool, Block)
    mungeMainBlock =
      if boardMainBlock.x == 0
      then (True, { boardMainBlock | x = winPosition })
      else if boardMainBlock.y == 0
      then (True, { boardMainBlock | y = winPosition })
      else if boardMainBlock.z == 0
      then (True, { boardMainBlock | z = winPosition })
      else (False, boardMainBlock)
    winner = Tuple.first mungeMainBlock

    mainBlockStyle_ = if winner
      then mainBlockStyle ++ [("opacity", "0")]
      else mainBlockStyle
    mainBlock = blockToBox (Tuple.second mungeMainBlock) mainBlockStyle_
    otherBlocks = List.concatMap (\block -> blockToBox block otherBlockStyle) board.blocks
  in
    div [Html.Attributes.style mainDivStyle] (boundingBox ++ mainBlock ++ otherBlocks)
  
blockToBox : Block -> Style -> List (Html msg)
blockToBox { length, x, y, z, orientation } =
  case orientation of
    X -> box x y z (x+length) (y+1) (z+1)
    Y -> box x y z (x+1) (y+length) (z+1)
    Z -> box x y z (x+1) (y+1) (z+length)

box : Int -> Int -> Int -> Int -> Int -> Int -> Style -> List (Html msg)
box x1 y1 z1 x2 y2 z2 style = [
    div [Html.Attributes.style ([width (y2 - y1), height (x2 - x1), transform Z y1 x1 z1] ++ style)] [],
    div [Html.Attributes.style ([width (y2 - y1), height (x2 - x1), transform Z y1 x1 z2] ++ style)] [],
    div [Html.Attributes.style ([width (z2 - z1), height (x2 - x1), transform Y z1 x1 y1] ++ style)] [],
    div [Html.Attributes.style ([width (z2 - z1), height (x2 - x1), transform Y z1 x1 y2] ++ style)] [],
    div [Html.Attributes.style ([width (y2 - y1), height (z2 - z1), transform X y1 z1 x1] ++ style)] [],
    div [Html.Attributes.style ([width (y2 - y1), height (z2 - z1), transform X y1 z1 x2] ++ style)] []
  ]
