#!/bin/bash -e
docker run -v $(pwd)/client:/code -w "/code" mblouin/docker_lesscss style.less style.css
docker run -v $(pwd)/client:/code -w "/code" codesimple/elm:0.18 make Main.elm --yes --output bricks.js
docker build -f server/Dockerfile.haskell.cabal -t haskell-image .
docker run -v $(pwd)/server:/code -w "/code" haskell-image ghc server.hs -optl-static -optl-pthread
docker build -t bricks:latest .


# Build and start:
#   sh -e build.sh && docker run -p 80:80 -it bricks:latest
# Stop with ^C
