FROM ubuntu:12.04

COPY client/static/* ./
COPY server/server .

CMD ./server
