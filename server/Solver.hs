module Solver where

import Board
import Data.Set (Set)
import qualified Data.Set as Set
import Data.List

-- TODO: Better type names for Coords and Set Coords
type Coords = (Int, Int, Int)

coordinates :: Block -> Coords
coordinates b = (x b, y b, z b)

getCoord :: Coords -> Orientation -> Int
getCoord (x, y, z) orientation =
  case orientation of
    X -> x
    Y -> y
    Z -> z

withCoord :: Coords -> Orientation -> Int -> Coords
withCoord (x, y, z) orientation newCoord =
  case orientation of
    X -> (newCoord, y, z)
    Y -> (x, newCoord, z)
    Z -> (x, y, newCoord)

advanceBy :: Coords -> Orientation -> Int -> Coords
advanceBy coords orientation delta =
  let
    oldCoord = getCoord coords orientation
    newCoord = oldCoord + delta
  in
    withCoord coords orientation newCoord

advanceBlockBy :: Block -> Int -> Block
advanceBlockBy block delta =
  case orientation block of
    X -> block { x = x block + delta }
    Y -> block { y = y block + delta }
    Z -> block { z = z block + delta }

spaces :: Block -> Set Coords
spaces block =
  let
    coords = coordinates block
    start = getCoord coords $ orientation block
    end = start + len block - 1
  in
    Set.fromList $ map (\x -> withCoord coords (orientation block) x) $ [start..end]

allSpaces :: Board -> Set Coords
allSpaces board = Set.unions $ (spaces $ mainBlock board) : (map spaces $ blocks board)

spaceEmpty :: Board -> Coords -> Bool
spaceEmpty board (x, y, z) =
  let
    usedSpaces = allSpaces board
  in
    (x >= 0) && (y >= 0) && (z >= 0) &&
    (x < width board) && (y < depth board) && (z < height board) &&
    (not $ Set.member (x, y, z) usedSpaces)

move :: Board -> Block -> [Block]
move board block =
  let
    coords = coordinates block
    o = orientation block
    nextSpace = advanceBy coords o $ len block
    prevSpace = advanceBy coords o (-1)
    moveIf space delta =
      if spaceEmpty board space
      then [advanceBlockBy block delta]
      else []
  in
    (moveIf nextSpace 1) ++ (moveIf prevSpace (-1))

-- https://stackoverflow.com/a/10133429/226008
-- This is inefficient (O(N)), though for our purposes N will likely be small enough that it doesn't matter.
-- The lists should be Arrays or Sequences for better efficiency if N is large.
replaceAtIndex :: Int -> a -> [a] -> [a]
replaceAtIndex n item ls = a ++ (item:b) where (a, (_:b)) = splitAt n ls

nextMoves :: Board -> [Board]
nextMoves board =
  let
    blocks' :: [Block]
    blocks' = mainBlock board : blocks board

    tryToMoveBlock :: Int -> [[Block]]
    tryToMoveBlock i =
      map (\block -> replaceAtIndex i block blocks') $ move board (blocks' !! i)

    tryToMoveAllBlocks :: [[Block]]
    tryToMoveAllBlocks = concatMap tryToMoveBlock [0..length blocks'-1]

    blocksToBoard :: [Block] -> Board
    blocksToBoard blocks' =
      board { mainBlock = head blocks', blocks = tail blocks' }

  in
    map (\blockArray -> blocksToBoard blockArray) tryToMoveAllBlocks

winner :: Board -> Bool
winner board =
    x b == 0 || y b == 0 || z b == 0
  where b = mainBlock board

foo = [0..]

solveBoard :: Board -> [Board]
solveBoard board =
  let
    -- An element in the queue is a board plus the index of its parent position
    queue :: [(Board, Int)]

    -- This implementation will not ignore previously-visited nodes.
    -- TODO: Figure out an elegant way to make it do that. No cheating and looking it up. :-)
    queue = (board, -1) : (concatMap nextSteps $ zip [0..] $ map fst queue)

    nextSteps :: (Int, Board) -> [(Board, Int)]
    nextSteps (parent, board) =
      map (\b -> (b, parent)) $ nextMoves board

    firstWinner :: Maybe (Board, Int)
    firstWinner = find (\(b, _) -> winner b) queue

    backtrack :: (Board, Int) -> [Board]
    backtrack (b, -1) = [b]
    backtrack (b, i)  = b : backtrack (queue !! i)
  in
    case firstWinner of
      Nothing -> []
      Just x  -> reverse $ backtrack x
