package main

type Opt[A any] interface { implOpt() }


// Data Constructors Structs
type Some[A any] struct { V A }
type None[A any] struct {  }


// Implement the interface
func (_ Some[A]) implOpt() {}
func (_ None[A]) implOpt() {}


// Constructors
func NewSome[A any](V A) Opt[A] { return Some[A]{ V: V } }
func NewNone[A any]() Opt[A] { return None[A]{  } }
