package main

type Bool interface { implBool() }


// Data Constructors Structs
type True struct {  }
type False struct {  }


// Implement the interface
func (_ True) implBool() {}
func (_ False) implBool() {}


// Constructors
func NewTrue() Bool { return True{  } }
func NewFalse() Bool { return False{  } }
