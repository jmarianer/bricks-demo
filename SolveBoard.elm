module SolveBoard exposing (..) --(solveBoard)

import Board exposing (Board, Block, Orientation(..), toBoard, serializeBoard)
import Set exposing (Set)

testBoard = toBoard "334;2112Y;2010Y;2200Y;2101Z"

spaces : Block -> Set (Int, Int, Int)
spaces block =
  case block.orientation of
    X -> Set.fromList <| List.map (\x -> (x, block.y, block.z)) <| List.range block.x (block.x + block.length - 1)
    Y -> Set.fromList <| List.map (\y -> (block.x, y, block.z)) <| List.range block.y (block.y + block.length - 1)
    Z -> Set.fromList <| List.map (\z -> (block.x, block.y, z)) <| List.range block.z (block.z + block.length - 1)

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
