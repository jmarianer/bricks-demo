FROM ubuntu:12.04

COPY client/index.html .
COPY client/style.css .
COPY client/bricks.js .
COPY server/server .

CMD ./server
