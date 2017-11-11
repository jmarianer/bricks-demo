module ShowBoard exposing (toHtml)

import Array
import Board exposing (Board, Block, Orientation(..))
import Html exposing (Html, div)
import StyleUtils exposing (..)

toHtml : Board -> Html msg
toHtml board =
  let
    mainDivStyle = [
      width board.width,
      height board.depth
    ]

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
    mainBlockClasses = if winner
      then [Rotatable, MainBlock, Invisible]
      else [Rotatable, MainBlock]

    windowOut_ = case boardMainBlock.orientation of
      X -> pane 1 1 X boardMainBlock.y boardMainBlock.z 0
      Y -> pane 1 1 Y boardMainBlock.z boardMainBlock.x 0
      Z -> pane 1 1 Z boardMainBlock.y boardMainBlock.x 0
    windowOut = [windowOut_ [Rotatable, WindowOut]]

    boundingBox = box 0 0 0 board.width board.depth board.height [Rotatable, BoundingBox]
    mainBlock = blockToBox (Tuple.second mungeMainBlock) mainBlockClasses
    otherBlocks = List.concatMap (\block -> blockToBox block [Rotatable, OtherBlock]) <| Array.toList board.blocks
  in
    div [classes [BlocksContainer], style mainDivStyle] (boundingBox ++ mainBlock ++ otherBlocks ++ windowOut)
  
blockToBox : Block -> List CssClass -> List (Html msg)
blockToBox { length, x, y, z, orientation } =
  case orientation of
    X -> box x y z (x+length) (y+1) (z+1)
    Y -> box x y z (x+1) (y+length) (z+1)
    Z -> box x y z (x+1) (y+1) (z+length)

box : Int -> Int -> Int -> Int -> Int -> Int -> List CssClass -> List (Html msg)
box x1 y1 z1 x2 y2 z2 classList = [
    pane (y2 - y1) (x2 - x1) Z y1 x1 z1 (BackPane::classList),
    pane (y2 - y1) (x2 - x1) Z y1 x1 z2 classList,
    pane (z2 - z1) (x2 - x1) Y z1 x1 y1 classList,
    pane (z2 - z1) (x2 - x1) Y z1 x1 y2 (BackPane::classList),
    pane (y2 - y1) (z2 - z1) X y1 z1 x1 (BackPane::classList),
    pane (y2 - y1) (z2 - z1) X y1 z1 x2 classList
  ]

pane w h orientation x y z classList =
  div [style [width w, height h, transform orientation x y z], classes classList] []

