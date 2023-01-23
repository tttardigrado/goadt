module Main where


import Test.HUnit (runTestTT, Counts (failures), Test (TestList, TestCase), assertEqual)
import System.Exit ( exitSuccess, exitFailure )
import Adt 
import Parser (parseAdt)

testParser :: String -> Adt -> Test
testParser s a = TestCase $ assertEqual "Parser Test" (parseAdt s) (Right a)

tests :: Test
tests = TestList
  [ -- parser tests
    testParser "Opt[A] = Some(V: A) | None()" Adt{name="Opt", params=["A"], rules=[Rule{tag="Some", fields=[Field{fid="V", fty="A"}]}, Rule{tag="None", fields=[]}]}
  , testParser "Either[A; B] = Left(Lft: A) | Right(Rgt: B)" Adt{name="Either", params=["A","B"], rules=[Rule{tag="Left", fields=[Field{fid="Lft", fty="A"}]}, Rule{tag="Right", fields=[Field{fid="Rgt", fty="B"}]}]}
  , testParser "Bool[] = True() | False()" Adt{name="Bool", params=[], rules=[Rule{tag="True", fields=[]}, Rule{tag="False", fields=[]}]}
  , testParser "Nat[] = Zero() | Succ(N: Nat)" Adt{name="Nat", params=[], rules=[Rule{tag="Zero", fields=[]}, Rule{tag="Succ", fields=[Field{fid="N", fty="Nat"}]}]}
  , testParser "Int[] = Val(i: int) | Add(a: Int; b: Int) | Mul(a: Int; b: Int) | Sum(as: []Int)" Adt {name="Int", params=[], rules=[Rule{tag="Val", fields=[Field{fid="i", fty="int"}]},Rule{tag="Add", fields=[Field{fid="a", fty="Int"}, Field{fid="b", fty="Int"}]},Rule{tag="Mul", fields=[Field{fid="a", fty="Int"}, Field{fid="b", fty="Int"}]},Rule{tag="Sum", fields=[Field{fid="as", fty="[]Int"}]}]}
  ]

main :: IO ()
main = do
    result1 <- runTestTT tests
    if failures result1 > 0 then exitFailure else exitSuccess