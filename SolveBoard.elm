module SolveBoard exposing (..) --(solveBoard)

import Board exposing (Board, Block, Orientation(..), toBoard, serializeBoard)
import Set exposing (Set)

testBoard = toBoard "334;2112Y;2010Y;2200Y;2101Z"

-- TODO: Better type names for Coords and Set Coords
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

advanceBy : Coords -> Orientation -> Int -> Coords
advanceBy coords orientation delta =
  let
    oldCoord = getCoord coords orientation
    newCoord = oldCoord + delta
  in
    withCoord coords orientation newCoord

advanceBlockBy : Block -> Int -> Block
advanceBlockBy block delta =
  case block.orientation of
    X -> { block | x = block.x + delta }
    Y -> { block | y = block.y + delta }
    Z -> { block | z = block.z + delta }

spaces : Block -> Set Coords
spaces block =
  let
    coords = coordinates block
    start = getCoord coords block.orientation
    end = start + block.length - 1
  in
    Set.fromList <| List.map (\x -> withCoord coords block.orientation x) <| List.range start end

allSpaces : Board -> Set Coords
allSpaces board = List.foldl Set.union (spaces board.mainBlock) (List.map spaces board.blocks)

spaceEmpty : Board -> Coords -> Bool
spaceEmpty board (x, y, z) =
  let
    usedSpaces = allSpaces board
  in
    (x >= 0) && (y >= 0) && (z >= 0) &&
    (x < board.width) && (y < board.depth) && (z < board.height) &&
    (not <| Set.member (x, y, z) usedSpaces)

move : Board -> Block -> List Block
move board block =
  let
    coords = coordinates block
    o = block.orientation
    nextSpace = advanceBy coords o block.length
    prevSpace = advanceBy coords o -1
    moveIf space delta =
      if spaceEmpty board space
      then [advanceBlockBy block delta]
      else []
  in
    (moveIf nextSpace 1) ++ (moveIf prevSpace -1)

--nextMoves : Board -> List Board
--nextMoves board =

--solveBoard : Board -> List Board
