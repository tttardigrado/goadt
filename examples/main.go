package main

import "fmt"

//go:generate goadt -p main -a "Opt[A] = Some(V: A) | None()"

//go:generate goadt -p main -a "Either[A; B] = Left(Lft: A) | Right(Rgt: B)"

//go:generate goadt -p main -a "Bool[] = True() | False()"

//go:generate goadt -p main -a "Nat[] = Zero() | Succ(N: Nat)"

//go:generate goadt -p main -a "Int[] = Val(i: int) | Add(a: Int; b: Int) | Mul(a: Int; b: Int)"

func main() {
	fmt.Println("Hello, World!")
}
