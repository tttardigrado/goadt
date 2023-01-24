package main


//go-sumtype:decl Int
type Int interface { implInt() }


// Data Constructors Structs
type Val struct { i int }
type Add struct { a Int; b Int }
type Mul struct { a Int; b Int }


// Implement the interface
func (_ Val) implInt() {}
func (_ Add) implInt() {}
func (_ Mul) implInt() {}


// Constructors
func NewVal(i int) Int { return Val{ i: i } }
func NewAdd(a Int, b Int) Int { return Add{ a: a, b: b } }
func NewMul(a Int, b Int) Int { return Mul{ a: a, b: b } }
