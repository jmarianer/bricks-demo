module Stylesheet exposing (CssClass(..), cssString)

import Css exposing (..)
import Css.Elements exposing (img, body, div, h1)

type CssClass = Rotatable | MainBlock | OtherBlock | WindowOut | Invisible | BoundingBox
css =
  stylesheet [
    everything [
      boxSizing borderBox
    ],
    class Rotatable [
      property "transform-origin" "top left",
      position absolute,
      property "transition" "all ease-in-out .3s"
    ],
    class BoundingBox [
      border3 (px 1) solid (hex "000")
    ],
    class MainBlock [
      border3 (px 1) solid (hex "000"),
      backgroundColor (hex "F00")
    ],
    class OtherBlock [
      border3 (px 1) solid (hex "000"),
      backgroundColor (hex "00F")
    ],
    class WindowOut [
      border3 (px 2) solid (hex "000")
    ],
    class Invisible [
      opacity (num 0)
    ]
  ]

cssString = .css <| Css.compile [css]
