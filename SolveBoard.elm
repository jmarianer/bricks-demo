module SolveBoard exposing (..) --(solveBoard)

import Board exposing (Board, Block, Orientation(..), toBoard, serializeBoard)
import Set exposing (Set)

testBoard = toBoard "334;2112Y;2010Y;2200Y;2101Z"

type alias Coords = (Int, Int, Int)

coordinates : Block -> Coords
coordinates { x, y, z } = (x, y, z)

getCoord : Coords -> Orientation -> Int
getCoord (x, y, z) orientation =
  case orientation of
    X -> x
    Y -> y
    Z -> z

withCoord : Coords -> Orientation -> Int -> Coords
withCoord (x, y, z) orientation newCoord =
  case orientation of
    X -> (newCoord, y, z)
    Y -> (x, newCoord, z)
    Z -> (x, y, newCoord)

spaces : Block -> Set Coords
spaces block =
  let
    coords = coordinates block
    start = getCoord coords block.orientation
    end = start + block.length - 1
  in
    Set.fromList <| List.map (\x -> withCoord coords block.orientation x) <| List.range start end



allSpaces : Board -> Set (Int, Int, Int)
allSpaces board = List.foldl Set.union (spaces board.mainBlock) (List.map spaces board.blocks)

{-
move : Board -> Block -> List Block
move board block =
  case block.orientation of
    X -> maybeMoveTo

--nextMoves : Board -> List Board
--nextMoves board =

--solveBoard : Board -> List Board
-}
