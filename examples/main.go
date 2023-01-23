package main

import "fmt"

//go:generate goadt -a "Opt[A] = Some(V: A) | None()"

//go:generate goadt -a "Either[A; B] = Left(Lft: A) | Right(Rgt: B)"

//go:generate goadt -a "Bool[] = True() | False()"

//go:generate goadt -a "Nat[] = Zero() | Succ(N: Nat)"

//go:generate goadt -a "Int[] = Val(i: int) | Add(a: Int; b: Int) | Mul(a: Int; b: Int)"

func main() {
	fmt.Println("Hello, World!")
}
