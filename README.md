# GoADT

![Tests CI](https://github.com/tttardigrado/goadt/actions/workflows/tests.yml/badge.svg)
![Build CI](https://github.com/tttardigrado/goadt/actions/workflows/build.yml/badge.svg)
![License](https://img.shields.io/github/license/tttardigrado/goadt)
<a href="https://twitter.com/intent/tweet?text=Check out goadt by %40_tardigrado_ https%3A%2F%2Fgithub.com%2Ftttardigrado%2Fgoadt 😁"><img src="https://img.shields.io/twitter/url?style=social&url=https%3A%2F%2Fgithub.com%2Ftttardigrado%2Fgoadt"></a>

Algebraic data types generator for the Go programming language

![Logo](./logo.png)

## Tech stack
* Haskell
* Golang
* Parsec
* CmdArgs

## Product types
A product type `A x B` is a compounded type that combines one element from each of the types. It corresponds to logical `AND`.

In golang, we are going to represent product types as `structs`.

```haskell
-- haskell
data Point = Point
  { x :: Int
  , y :: Int
  }
```

```go
// golang
type Point struct {
    X int
    Y int
}
```

## Sum types
A sum type `A + B` is a compounded type that requires one element from one of the types (corresponds to logical `OR`).

In golang, we are going to represent product types as `interfaces`
```haskell
-- haskell
data BOOL = TRUE | FALSE
```
```go
// go
type BOOL infterface {
    implBOOL()
}

type TRUE struct {}
func (_ TRUE) implBOOL() {}

type FALSE struct {}
func (_ FALSE) implBOOL() {}
```

## Using GoADT
Check the [examples](./examples/) directory to see how one could use `goadt` and `go generate` to automatically generate ADTs

## References:
1. [Go and Algebraic data types](https://eli.thegreenplace.net/2018/go-and-algebraic-data-types/) by Eli Bendersky
2. [Sum Types in Go](https://www.jerf.org/iri/post/2917/) by Jeremy Bowers
3. [Golang Tips #1: ADT](https://rguilmont.net/blog/2022-02-20-golang-generics-options/) by Romain Guilmont
3. [Alternatives to sum types in Go](https://making.pusher.com/alternatives-to-sum-types-in-go/) by Will Sewell
4. [Wikipedia's](https://en.wikipedia.org/wiki/Algebraic_data_type) article on ADTs
5. [Algebraic Data Types in Haskell](https://serokell.io/blog/algebraic-data-types-in-haskell) by Gints Dreimanis 
6. [Why algebraic data types are important](https://www.youtube.com/watch?v=LkqTLJK2API) by Bartosz Milewski
7. [go-sumtype](https://github.com/BurntSushi/go-sumtype) an exhaustiveness checker for sum types by Andrew Gallant