# GoADT

![Tests CI](https://github.com/tttardigrado/goadt/actions/workflows/tests.yml/badge.svg)
![Build CI](https://github.com/tttardigrado/goadt/actions/workflows/build.yml/badge.svg)
![License](https://img.shields.io/github/license/tttardigrado/goadt)
<a href="https://twitter.com/intent/tweet?text=Check out goadt, a Golang Algebraic Data Type generator written by %40_tardigrado_ in Haskell https%3A%2F%2Fgithub.com%2Ftttardigrado%2Fgoadt 😁"><img src="https://img.shields.io/twitter/url?style=social&url=https%3A%2F%2Fgithub.com%2Ftttardigrado%2Fgoadt"></a>

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
data Tuple a b = Tuple
  { x :: a
  , y :: b
  }
```

```go
// golang
type Tuple[A, B any] struct {
    x A
    y B
}
```

## Sum types
A sum type `A + B` is a compounded type that requires one element from one of the types (corresponds to logical `OR`).

In golang, we are going to represent product types as `interfaces`
```haskell
-- haskell
data Either a b
  = Left a
  | Right b
```
```go
// go
type Either[A, B any] infterface {
    implEither()
}

type Left[A, B any] struct { a A }
func (_ Left) implEither() {}

type Right[A, B any] struct { b B }
func (_ Right) implEither() {}
```

## Using ADTs

Let's start with an example of an ADT: `Opt`. This type represents the possibility of the non-existence of a value (in Haskell it's known as `Maybe`, but we will define an isomorphic `Opt` type)

```haskell
-- haskell
data Opt a
  = None
  | Some a
```
```go
// go
type Opt[A any] interface { implOpt() }

// Data Constructors Structs
type Some[A any] struct { V A }
type None[A any] struct {     }

// Implement the interface
func (_ Some[A]) implOpt() { }
func (_ None[A]) implOpt() { }

// Constructors
func NewSome[A any](V A) Opt[A] { return Some[A]{ V: V } }
func NewNone[A any](   ) Opt[A] { return None[A]{      } }
```

Now, let's see how to use it. In most functional programming languages (like Haskell, Ocaml and SML) ADTs can be used with `pattern matching`, but, unfortunately, go does not have that feature. The closest analog is a `type switch`.

```haskell
-- haskell
map :: (a -> r) -> Opt a -> Opt r
map fn (Some v) = Some $ fn v
map fn  None    = None
```

```go
// go
func Map[A, R any](fn func(A) R, opt Opt[A]) (res Opt[R]) {
	switch t := opt.(type) {
	case Some[A]: res = NewSome(fn(t.V))
	case None[A]: res = NewNone[R]()
	}
	return
}
```

If you build your functions this way, [go-sumtype](https://github.com/BurntSushi/go-sumtype) can be used to check for the exhaustiveness of the type switches. 

## Using GoADT
Check the [examples](./examples/) directory to see how one could use `goadt` and `go generate` to automatically generate ADTs

## References:
1. [Go and Algebraic data types](https://eli.thegreenplace.net/2018/go-and-algebraic-data-types/) by Eli Bendersky
2. [Sum Types in Go](https://www.jerf.org/iri/post/2917/) by Jeremy Bowers
3. [Golang Tips #1: ADT](https://rguilmont.net/blog/2022-02-20-golang-generics-options/) by Romain Guilmont
4. [Alternatives to sum types in Go](https://making.pusher.com/alternatives-to-sum-types-in-go/) by Will Sewell
5. [Wikipedia's](https://en.wikipedia.org/wiki/Algebraic_data_type) article on ADTs
6. [Algebraic Data Types in Haskell](https://serokell.io/blog/algebraic-data-types-in-haskell) by Gints Dreimanis 
7. [Why algebraic data types are important](https://www.youtube.com/watch?v=LkqTLJK2API) by Bartosz Milewski
8. [go-sumtype](https://github.com/BurntSushi/go-sumtype) an exhaustiveness checker for sum types by Andrew Gallant