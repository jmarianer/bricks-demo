{-# LANGUAGE OverloadedStrings #-}

import Network.Wai
import Network.Wai.Handler.Warp
import Network.HTTP.Types (status200, status404)
import qualified Data.Text
import qualified Data.ByteString.Builder
import qualified Data.Map as Map

import Board
import Data.List
import Data.Maybe
import Solver

files = [
  ("index.html", "text/html"),
  ("style.css", "text/css"),
  ("bricks.js", "application/javascript"),
  ("Trash.svg", "image/svg+xml")]

readOneFile (filename, mimeType) = do
  contents <- readFile filename
  return (filename, (mimeType, contents))

main = do
    let port = 80
    staticFiles' <- sequence $ map readOneFile files
    let staticFiles = Map.fromList staticFiles'
    putStrLn $ "Listening on port " ++ show port
    run port (app staticFiles)
 
app staticFiles req respond = respond $
    case pathInfo req of
        ["solve", x] -> return200 "text/plain" $ intercalate "\n" $ map serializeBoard $ solveBoard $ fromJust $ toBoard $ Data.Text.unpack x
        [""]         -> returnIndex
        []           -> returnIndex
        [x]          -> case Map.lookup (Data.Text.unpack x) staticFiles of
          Just (t, d)  -> return200 t d
          Nothing      -> return404
  where
    (_, index) = staticFiles Map.! "index.html"
    returnIndex = return200 "text/html" index
 
return200 mimeType contents = responseBuilder status200 [ ("Content-Type", mimeType) ] $ Data.ByteString.Builder.stringUtf8 contents
return404 = responseBuilder status404 [] $ Data.ByteString.Builder.stringUtf8 ""
