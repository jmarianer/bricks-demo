{-# LANGUAGE OverloadedStrings #-}

import Network.Wai
import Network.Wai.Handler.Warp
import Network.HTTP.Types (status200)
import qualified Data.Text
import qualified Data.ByteString.Builder
 
main = do
    let port = 80
    index <- readFile "index.html"
    putStrLn $ "Listening on port " ++ show port
    run port (app index)
 
app index req respond = respond $
    case pathInfo req of
        [x] -> return200 ("<p>Hello from " ++ (Data.Text.unpack x) ++ "!</p>")
        _ -> return200html index
 
return200html string = responseBuilder status200 [ ("Content-Type", "text/html") ] $ Data.ByteString.Builder.stringUtf8 string
return200 string = responseBuilder status200 [ ("Content-Type", "text/plain") ] $ Data.ByteString.Builder.stringUtf8 string
