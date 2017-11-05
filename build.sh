#!/bin/bash -e
docker run -v $(pwd)/client:/code -w "/code" codesimple/elm:0.18 make Main.elm --yes
HASKELL_IMAGE=$(docker build -qf server/Dockerfile.haskell.cabal .)
docker run -v $(pwd)/server:/code -w "/code" ${HASKELL_IMAGE/sha256:/} ghc server.hs -optl-static -optl-pthread
docker build -t bricks:latest .


# Build and start:
#   sh -e build.sh && docker run --name foo -p 80:80 bricks:latest
# Stop:
#   docker stop -t0 foo && docker rm foo
