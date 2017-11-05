#!/bin/bash
docker run -v $(pwd)/client:/code -w "/code" codesimple/elm:0.18 make Main.elm
docker build -t bricks:latest .
