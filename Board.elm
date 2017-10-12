module Board exposing (Board, Block, Orientation(..), toBoard)

type Orientation = X | Y | Z

type alias Block = {
  length: Int,
  x: Int,
  y: Int,
  z: Int,
  orientation: Orientation
}

type alias Board = {
  width: Int,
  depth: Int,
  height: Int,
  mainBlock: Block,
  blocks: List Block
}


-- toBoard converts a string to a Board following a specific format. The
-- function will crash if the string is improperly formatted.

-- Utility functions to help with the crashing
fromJust : Maybe a -> a
fromJust x = case x of
    Just y -> y
    Nothing -> Debug.crash "error: fromJust Nothing"

definitelyInt s = String.toInt s |> Result.toMaybe |> fromJust

toOrientation s = case s of
  "X" -> X
  "Y" -> Y
  "Z" -> Z
  _   -> Debug.crash <| "error: toOrientation " ++ s



toBoard : String -> Board
toBoard s =
  let
    toBlock : String -> Block
    toBlock s = {
      length =      definitelyInt <| String.slice 0 1 s,
      x =           definitelyInt <| String.slice 1 2 s,
      y =           definitelyInt <| String.slice 2 3 s,
      z =           definitelyInt <| String.slice 3 4 s,
      orientation = toOrientation <| String.slice 4 5 s
    }

    splits = String.split ";" s
    boundary = fromJust <| List.head splits
    blocks = List.map toBlock <| fromJust <| List.tail splits
  in {
    width     = definitelyInt <| String.slice 0 1 s,
    depth     = definitelyInt <| String.slice 1 2 s,
    height    = definitelyInt <| String.slice 2 3 s,
    mainBlock = fromJust <| List.head blocks,
    blocks    = fromJust <| List.tail blocks
  }
