module Board where

import Control.Monad
import Data.Char
import Data.List

data Orientation = X | Y | Z deriving (Show, Eq)

data Block = Block {
  len :: Int,
  x :: Int,
  y :: Int,
  z :: Int,
  orientation :: Orientation
} deriving (Show, Eq)

data Board = Board {
  -- TODO: Consider renaming to maxX, maxY, maxZ or a tuple of ints.
  width :: Int,
  depth :: Int,
  height :: Int,
  mainBlock :: Block,
  blocks :: [Block]
} deriving (Show, Eq)

toInt :: String -> Int -> Maybe Int
toInt s charIndex =
  if isDigit c
    then Just $ digitToInt c
    else Nothing
  where c = s !! charIndex

toOrientation :: Char -> Maybe Orientation
toOrientation s = case s of
  'X' -> Just X
  'Y' -> Just Y
  'Z' -> Just Z
  _   -> Nothing


toBlock :: String -> Maybe Block
toBlock s = liftM5 (\l x y z o -> Block {
  len =         l,
  x =           x,
  y =           y,
  z =           z,
  orientation = o
  }) (toInt s 0) (toInt s 1) (toInt s 2) (toInt s 3) (toOrientation $ s !! 4)

-- https://stackoverflow.com/a/24792141/226008
split :: Eq a => a -> [a] -> [[a]]
split d [] = []
split d s = x : split d (drop 1 y) where (x,y) = span (/= d) s

toBoard :: String -> Maybe Board
toBoard s =
  case split ';' s of
    boundary:mainBlock:blocks ->
      liftM5 (\w d h mb b -> Board {
        width     = w,
        depth     = d,
        height    = h,
        mainBlock = mb,
        blocks    = b
        }) (toInt boundary 0) (toInt boundary 1) (toInt boundary 2) (toBlock mainBlock) (sequence $ map toBlock blocks)
    _                           -> Nothing

serializeBoard :: Board -> String
serializeBoard b =
  let
    convertBlock :: Block -> String
    convertBlock b = concat [show $ len b, show $ x b, show $ y b, show $ z b, show $ orientation b]

    init = concat [show $ width b, show $ depth b, show $ height b]
    convertedBlocks = map convertBlock ((mainBlock b) : (blocks b))
  in
    intercalate ";" (init : convertedBlocks)
