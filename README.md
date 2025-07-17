# Bricks demo

This is written in Elm and Haskell. The deploy is self-contained:

```sh
docker build . -t joeym/bricks:latest
docker run -p 80:80 -it joeym/bricks:latest
```

There is no easy way to change anything live. When I rewrite it all in React...