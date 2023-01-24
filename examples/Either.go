package main


//go-sumtype:decl Either
type Either[A, B any] interface { implEither() }


// Data Constructors Structs
type Left[A, B any] struct { Lft A }
type Right[A, B any] struct { Rgt B }


// Implement the interface
func (_ Left[A, B]) implEither() {}
func (_ Right[A, B]) implEither() {}


// Constructors
func NewLeft[A, B any](Lft A) Either[A, B] { return Left[A, B]{ Lft: Lft } }
func NewRight[A, B any](Rgt B) Either[A, B] { return Right[A, B]{ Rgt: Rgt } }
