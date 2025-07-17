FROM mblouin/docker_lesscss AS build_css
COPY client /code
WORKDIR /code
RUN lessc style.less style.css

FROM codesimple/elm:0.18 AS build_elm
COPY client /code
WORKDIR /code
RUN elm make Main.elm --yes --output bricks.js

FROM haskell:8 AS build_haskell
RUN cabal update
RUN cabal install --lib http-types
RUN cabal install --lib wai
RUN cabal install --lib warp
RUN cabal install --lib utf8-string
COPY server /code
WORKDIR /code
RUN ghc server.hs -optl-static -optl-pthread

FROM alpine:20250108
COPY --from=build_css code/style.css ./
COPY --from=build_elm code/bricks.js ./
COPY --from=build_haskell code/server ./
COPY client/static/* .

CMD ./server
