# Simon in Haskell

A simple version of the classic [Simon game](https://en.wikipedia.org/wiki/Simon_(game)) written in Haskell using the graphics library [Gloss](https://hackage.haskell.org/package/gloss).

<p align="center">
  <img width="400" height="400" src="https://user-images.githubusercontent.com/3193712/44680089-2106e280-aa13-11e8-9f96-ce75b49aa489.gif">
</p>

The intention of this project was to serve as a learning exercise for myself. It's my first Haskell project, so the code is probably terrible.

## Building and running

The project can be built and run using [Stack](https://docs.haskellstack.org/en/stable/README/#how-to-install). Here's how to get started:

1. Clone the project and change to the project directory.
```
$ git clone https://github.com/gustavohb/simon.git
$ cd simon
```
2. Build the project with `stack`.
```
$ stack build --install-ghc
```
3. Run and play!
```
$ stack exec simon
```
