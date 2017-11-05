FROM ubuntu:12.04

COPY client/index.html .
COPY server/server .

CMD ./server
