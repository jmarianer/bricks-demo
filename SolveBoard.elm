module SolveBoard exposing (solveBoard)

import Array exposing (Array)
import Board exposing (Board, Block, Orientation(..), toBoard, serializeBoard)
import Dict exposing (Dict)
import Set exposing (Set)

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
allSpaces board = List.foldl Set.union (spaces board.mainBlock) (List.map spaces <| Array.toList board.blocks)

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

fromJust x = case x of
    Just y -> y
    Nothing -> Debug.crash "error: fromJust Nothing"

nextMoves : Board -> List Board
nextMoves board =
  let
    blocks : Array Block
    blocks = Array.push board.mainBlock board.blocks

    tryToMoveBlock : Int -> List (Array Block)
    tryToMoveBlock i =
      List.map (\block -> Array.set i block blocks) <| move board (fromJust <| Array.get i blocks)

    tryToMoveAllBlocks : List (Array Block)
    tryToMoveAllBlocks = List.concatMap tryToMoveBlock <| List.range 0 <| Array.length blocks - 1

    blocksToBoard : Array Block -> Board
    blocksToBoard blocks =
      { board | mainBlock = fromJust <| Array.get (Array.length blocks - 1) blocks,
                blocks    = Array.slice 0 -1 blocks }

  in
    List.map (\blockArray -> blocksToBoard <| blockArray) tryToMoveAllBlocks

winner : Board -> Bool
winner board =
  board.mainBlock.x == 0 || board.mainBlock.y == 0 || board.mainBlock.z == 0
  
type alias SolverState = {
  queue : List String,
  previousPosition : Dict String String,
  visited : Set String
  }

iterate : SolverState -> (Bool, SolverState)
iterate state =
  let
    newState currentPosition restOfQueue =
      let
        nextBoards = List.map serializeBoard <| nextMoves <| toBoard currentPosition
        newBoards = Set.diff (Set.fromList nextBoards) state.visited
        newPrevs = List.map (\x -> (x, currentPosition)) <| Set.toList newBoards

        _ = Debug.log currentPosition ""
      in
        { state | queue = restOfQueue ++ Set.toList (newBoards),
                  previousPosition = Dict.union state.previousPosition <| Dict.fromList newPrevs,
                  visited = Set.union state.visited newBoards
        }
  in
    case state.queue of
      [] -> (True, state)
      x::xs ->
        if (winner <| toBoard x)
        then (True, state)
        else (False, newState x xs)

iterateUntilDone : SolverState -> SolverState
iterateUntilDone state =
  let
    nextIteration = iterate state
  in
    case nextIteration of
      (True,  nextState) -> nextState
      (False, nextState) -> iterateUntilDone nextState

extractPath : String -> SolverState -> List String
extractPath destination state =
  case Dict.get destination state.previousPosition of
    Nothing -> [destination]
    Just x  -> destination :: extractPath x state

extractDefaultPath : SolverState -> List String
extractDefaultPath state =
  case state.queue of
    [] -> []
    x::xs -> extractPath x state

solveBoard : Board -> List Board
solveBoard board =
  let
    initialState = {
      queue = [serializeBoard board],
      previousPosition = Dict.empty,
      visited = Set.singleton <| serializeBoard board
    }
    finalState = iterateUntilDone initialState
  in
    List.map toBoard <| List.reverse <| extractDefaultPath finalState
