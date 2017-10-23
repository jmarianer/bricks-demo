module Board exposing (Board, Block, Orientation(..), toBoard, serializeBoard)

import Array exposing (Array)

type Orientation = X | Y | Z

type alias Block = {
  length: Int,
  -- TODO: Consider a tuple of ints instead.
  x: Int,
  y: Int,
  z: Int,
  orientation: Orientation
}

type alias Board = {
  -- TODO: Consider renaming to maxX, maxY, maxZ or a tuple of ints.
  width: Int,
  depth: Int,
  height: Int,
  mainBlock: Block,
  blocks: Array Block
}


toInt : String -> Int -> Maybe Int
toInt s charIndex = String.slice charIndex (charIndex+1) s |> String.toInt |> Result.toMaybe

toOrientation : String -> Maybe Orientation
toOrientation s = case s of
  "X" -> Just X
  "Y" -> Just Y
  "Z" -> Just Z
  _   -> Nothing

-- TODO clever name
cleverName : List (Maybe a) -> Maybe (List a)
cleverName l = case l of
  []            -> Just []
  Nothing :: _  -> Nothing
  Just l  :: ls ->
    case cleverName ls of
      Nothing  -> Nothing
      Just ls_ -> Just (l :: ls_)

toBlock : String -> Maybe Block
toBlock s = Maybe.map5 (\l x y z o -> {
  length =      l,
  x =           x,
  y =           y,
  z =           z,
  orientation = o
  }) (toInt s 0) (toInt s 1) (toInt s 2) (toInt s 3) (toOrientation <| String.slice 4 5 s)

toBoard : String -> Maybe Board
toBoard s =
  case String.split ";" s of
    boundary::mainBlock::blocks ->
      Maybe.map5 (\w d h mb b -> {
        width     = w,
        depth     = d,
        height    = h,
        mainBlock = mb,
        blocks    = Array.fromList b
        }) (toInt boundary 0) (toInt boundary 1) (toInt boundary 2) (toBlock mainBlock) (cleverName <| List.map toBlock blocks)
    _                           -> Nothing

serializeBoard : Board -> String
serializeBoard b =
  let
    convertBlock : Block -> String
    convertBlock b = String.concat [toString b.length, toString b.x, toString b.y, toString b.z, toString b.orientation]

    init = String.concat [toString b.width, toString b.depth, toString b.height]
    convertedBlocks = List.map convertBlock (b.mainBlock :: Array.toList b.blocks)
  in
    String.join ";" (init :: convertedBlocks)
