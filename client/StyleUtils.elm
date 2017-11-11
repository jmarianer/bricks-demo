module StyleUtils exposing (..)

import Board exposing (Orientation(..))
import Css
import Html.Attributes
import Html.CssHelpers

type CssClass = BlocksContainer | Rotatable | MainBlock | OtherBlock | WindowOut | Invisible | BoundingBox | BackPane

style = Css.asPairs >> Html.Attributes.style

pixelsPerBlock = 50
toPixels x = Css.px <| toFloat <| pixelsPerBlock * x

width : Int -> Css.Style
width = Css.width << toPixels

height : Int -> Css.Style
height = Css.height << toPixels

transform : Orientation -> Int -> Int -> Int -> Css.Style
transform orientation x y z = 
  let
    translate x y z =
      Css.translate3d (toPixels x) (toPixels y) (toPixels z)

    transformList = case orientation of
      X -> [Css.rotateX (Css.deg -90), translate x y z]
      Y -> [Css.rotateY (Css.deg  90), translate x y z]
      Z -> [translate x y -z]
  in
    Css.transforms transformList

classes = .class <| Html.CssHelpers.withNamespace ""
