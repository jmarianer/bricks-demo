module Stylesheet exposing (CssClass(..), cssString)

import Css exposing (..)
import Css.Elements exposing (img, body, div, h1)

type CssClass = Rotatable | MainBlock | OtherBlock | WindowOut | Invisible | BoundingBox | BackPane
border = border3 (px 1) solid (hex "000")
mainBlockColor = hex "C00"
otherBlockColor = hex "1685CC"

css =
  stylesheet [
    body [
      backgroundColor (hex "85B4FF")
    ],
    everything [
      boxSizing borderBox,
      Css.transformStyle Css.preserve3d
    ],
    class Rotatable [
      property "transform-origin" "top left",
      position absolute,
      property "transition" "all ease-in-out .3s"
    ],
    class BoundingBox [
      border
    ],
    class MainBlock [
      border,
      backgroundColor mainBlockColor
    ],
    class OtherBlock [
      border,
      backgroundColor otherBlockColor
    ],
    class BackPane [
      backgroundColor (hex "FFF")
    ],
    class WindowOut [
      border3 (px 2) solid (hex "000")
    ],
    class Invisible [
      opacity (num 0)
    ]
  ]

cssString = .css <| Css.compile [css]
