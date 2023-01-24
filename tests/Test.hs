module Main where

import           Adt                           (Adt (..), Field (..), Rule (..))
import           Data.Either                   (isLeft)
import           Parser                        (adtP, fieldP, genP, idP,
                                                parseAdt, typeP, fieldsP, ruleP)
import           System.Exit                   (exitFailure, exitSuccess)
import           Test.HUnit                    (Counts (failures),
                                                Test (TestCase, TestList),
                                                assertBool, runTestTT)
import           Text.ParserCombinators.Parsec (Parser, parse)

testParser :: (Show a, Eq a) => Parser a -> String -> a -> Test
testParser p s r = TestCase $ assertBool "Individual Parser Test" $ parse p "" s == Right r

tests :: Test
tests = TestList
  [ -- idP tests
    testParser idP "a" "a"
  , testParser idP "A" "A"
  , testParser idP "a1" "a1"
  , testParser idP "A1" "A1"
  , testParser idP "a_" "a_"
  , testParser idP "A_" "A_"
  , testParser idP "ab" "ab"
  , testParser idP "Ab" "Ab"
  , testParser idP "aB" "aB"
  , testParser idP "AB" "AB"

  -- typeP tests
  , testParser typeP "int" "int"
  , testParser typeP "A[T]" "A[T]"
  , testParser typeP "A[T,R]" "A[T,R]"
  , testParser typeP "[]int" "[]int"
  , testParser typeP "[]A[T,R]" "[]A[T,R]"

  -- fieldP tests
  , testParser fieldP "a:A" Field{fid="a",fty="A"}
  , testParser fieldP "a:[]A" Field{fid="a",fty="[]A"}
  , testParser fieldP "a:A[T]" Field{fid="a",fty="A[T]"}
  , testParser fieldP "a:[]A[T,R]" Field{fid="a",fty="[]A[T,R]"}
  , testParser fieldP "a1:A" Field{fid="a1",fty="A"}
  , testParser fieldP "a_:A" Field{fid="a_",fty="A"}
  , testParser fieldP "aA:A" Field{fid="aA",fty="A"}

  -- genP tests
  , testParser genP "[a;A12;a_123]" ["a", "A12", "a_123"]
  , testParser genP "[a;A12]" ["a", "A12"]
  , testParser genP "[a]" ["a"]
  , testParser genP "[]" []

  -- fieldsP tests
  , testParser fieldsP "(a:int;a1:[]A;a_1:A[T,R];a_:[]A[T,R])" [Field "a" "int", Field "a1" "[]A", Field "a_1" "A[T,R]", Field "a_" "[]A[T,R]"]
  , testParser fieldsP "(a:int;a1:[]A;a_1:A[T,R])" [Field "a" "int", Field "a1" "[]A", Field "a_1" "A[T,R]"]
  , testParser fieldsP "(a:int;a1:[]A)" [Field "a" "int", Field "a1" "[]A"]
  , testParser fieldsP "(a:int)" [Field "a" "int"]
  , testParser fieldsP "()" []

  -- ruleP tests
  , testParser ruleP "Some(a:int;a1:[]A;a_1:A[T,R];a_:[]A[T,R])" Rule{tag="Some", fields=[Field "a" "int", Field "a1" "[]A", Field "a_1" "A[T,R]", Field "a_" "[]A[T,R]"]}
  , testParser ruleP "Some(a:int;a1:[]A;a_1:A[T,R])" Rule{tag="Some", fields=[Field "a" "int", Field "a1" "[]A", Field "a_1" "A[T,R]"]}
  , testParser ruleP "Some(a:int;a1:[]A)" Rule{tag="Some", fields=[Field "a" "int", Field "a1" "[]A"]}
  , testParser ruleP "Some(a:int)" Rule{tag="Some", fields=[Field "a" "int"]}
  , testParser ruleP "Some()" Rule{tag="Some", fields=[]}

  -- full parser tests
  , testParser adtP "Opt[A]=Some(V:A)|None()" Adt{name="Opt", params=["A"], rules=[Rule{tag="Some", fields=[Field{fid="V", fty="A"}]}, Rule{tag="None", fields=[]}]}
  , testParser adtP "Either[A;B]=Left(Lft:A)|Right(Rgt:B)" Adt{name="Either", params=["A","B"], rules=[Rule{tag="Left", fields=[Field{fid="Lft", fty="A"}]}, Rule{tag="Right", fields=[Field{fid="Rgt", fty="B"}]}]}
  , testParser adtP "Bool[]=True()|False()" Adt{name="Bool", params=[], rules=[Rule{tag="True", fields=[]}, Rule{tag="False", fields=[]}]}
  , testParser adtP "Nat[]=Zero()|Succ(N:Nat)" Adt{name="Nat", params=[], rules=[Rule{tag="Zero", fields=[]}, Rule{tag="Succ", fields=[Field{fid="N", fty="Nat"}]}]}
  , testParser adtP "Int[]=Val(i:int)|Add(a:Int;b:Int)|Mul(a:Int;b:Int)|Sum(as:[]Int)" Adt {name="Int", params=[], rules=[Rule{tag="Val", fields=[Field{fid="i", fty="int"}]},Rule{tag="Add", fields=[Field{fid="a", fty="Int"}, Field{fid="b", fty="Int"}]},Rule{tag="Mul", fields=[Field{fid="a", fty="Int"}, Field{fid="b", fty="Int"}]},Rule{tag="Sum", fields=[Field{fid="as", fty="[]Int"}]}]}
  ]

main :: IO ()
main = do
    result1 <- runTestTT tests
    if failures result1 > 0 then exitFailure else exitSuccess
