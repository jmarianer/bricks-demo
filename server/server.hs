{-# LANGUAGE OverloadedStrings #-}

import Network.Wai
import Network.Wai.Handler.Warp
import Network.HTTP.Types (status200)
import qualified Data.Text
import qualified Data.ByteString.Builder

import Board
import Data.List
import Data.Maybe
import Solver
 
main = do
    let port = 80
    index <- readFile "index.html"
    putStrLn $ "Listening on port " ++ show port
    run port (app index)
 
app index req respond = respond $
    case pathInfo req of
        [x] -> return200 $ intercalate "\n" $ map serializeBoard $ solveBoard $ fromJust $ toBoard $ Data.Text.unpack x
--"334;2112Y;2010Y;2200Y;2102Z\n334;2112Y;2010Y;2200Y;2101Z\n334;2112Y;2010Y;2200Y;2100Z\n334;2102Y;2010Y;2200Y;2100Z"
        _   -> return200html index
 
return200html string = responseBuilder status200 [ ("Content-Type", "text/html") ] $ Data.ByteString.Builder.stringUtf8 string
return200 string = responseBuilder status200 [ ("Content-Type", "text/plain") ] $ Data.ByteString.Builder.stringUtf8 string
