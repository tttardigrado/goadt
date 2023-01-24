package main


//go-sumtype:decl Nat
type Nat interface { implNat() }


// Data Constructors Structs
type Zero struct {  }
type Succ struct { N Nat }


// Implement the interface
func (_ Zero) implNat() {}
func (_ Succ) implNat() {}


// Constructors
func NewZero() Nat { return Zero{  } }
func NewSucc(N Nat) Nat { return Succ{ N: N } }
