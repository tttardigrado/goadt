module Main where

import           Adt                           (Adt (..), Field (..), Rule (..),
                                                body, construcct, gensDef,
                                                gensFun, impl, interface,
                                                struct)
import           Data.Either                   (isLeft)
import           Parser                        (adtP, fieldP, fieldsP, genP,
                                                idP, parseAdt, ruleP, typeP)
import           System.Exit                   (exitFailure, exitSuccess)
import           Test.HUnit                    (Counts (failures),
                                                Test (TestCase, TestList),
                                                assertEqual, runTestTT)
import           Text.ParserCombinators.Parsec (Parser, parse)

testParser :: (Show a, Eq a) => Parser a -> String -> a -> Test
testParser p s r = TestCase $ assertEqual "Individual Parser Test" (parse p "" s) (Right r)

testGen :: String -> String -> Test
testGen got want = TestCase $ assertEqual "Individual Generator Test" got want

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



  -- gensDef
  , testGen (gensDef $ Adt "" ["A", "B", "C1"] []) "[A, B, C1 any]"
  , testGen (gensDef $ Adt "" ["A", "B"] []) "[A, B any]"
  , testGen (gensDef $ Adt "" ["A"] []) "[A any]"
  , testGen (gensDef $ Adt "" [] []) ""

  -- gensFun
  , testGen (gensFun $ Adt "" ["A", "B", "C1"] []) "[A, B, C1]"
  , testGen (gensFun $ Adt "" ["A", "B"] []) "[A, B]"
  , testGen (gensFun $ Adt "" ["A"] []) "[A]"
  , testGen (gensFun $ Adt "" [] []) ""

  -- interface
  , testGen (interface $ Adt "Some" ["A", "B", "C1"] []) "type Some[A, B, C1 any] interface { implSome() }\n"
  , testGen (interface $ Adt "Some" ["A", "B"] []) "type Some[A, B any] interface { implSome() }\n"
  , testGen (interface $ Adt "Some" ["A"] []) "type Some[A any] interface { implSome() }\n"
  , testGen (interface $ Adt "Some" [] []) "type Some interface { implSome() }\n"

  -- struct
  , testGen (struct (Adt "Some" ["A", "B", "C1"] []) (Rule "Rule" [Field "a" "[]int", Field "b" "Opt[T]", Field "c" "*A"])) "type Rule[A, B, C1 any] struct { a []int; b Opt[T]; c *A }\n"
  , testGen (struct (Adt "Some" ["A", "B"] []) (Rule "Rule" [Field "a" "[]int", Field "b" "Opt[T]"])) "type Rule[A, B any] struct { a []int; b Opt[T] }\n"
  , testGen (struct (Adt "Some" ["A"] []) (Rule "Rule" [Field "a" "[]int"])) "type Rule[A any] struct { a []int }\n"
  , testGen (struct (Adt "Some" ["A"] []) (Rule "Rule" [])) "type Rule[A any] struct {  }\n"
  , testGen (struct (Adt "Some" [] []) (Rule "Rule" [Field "a" "[]int"])) "type Rule struct { a []int }\n"
  , testGen (struct (Adt "Some" [] []) (Rule "Rule" [])) "type Rule struct {  }\n"

  -- impl
  , testGen (impl (Adt "Some" ["A", "B", "C1"] []) (Rule "Rule" [])) "func (_ Rule[A, B, C1]) implSome() {}\n"
  , testGen (impl (Adt "Some" ["A", "B"] []) (Rule "Rule" [])) "func (_ Rule[A, B]) implSome() {}\n"
  , testGen (impl (Adt "Some" ["A"] []) (Rule "Rule" [])) "func (_ Rule[A]) implSome() {}\n"
  , testGen (impl (Adt "Some" ["A"] []) (Rule "Rule" [])) "func (_ Rule[A]) implSome() {}\n"
  , testGen (impl (Adt "Some" [] []) (Rule "Rule" [])) "func (_ Rule) implSome() {}\n"
  , testGen (impl (Adt "Some" [] []) (Rule "Rule" [])) "func (_ Rule) implSome() {}\n"

  -- construct
  , testGen (construcct (Adt "Some" ["A", "B", "C1"] []) (Rule "Rule" [Field "a" "[]int", Field "b" "Opt[T]", Field "c" "*A"])) "func NewRule[A, B, C1 any](a []int, b Opt[T], c *A) Some[A, B, C1] { return Rule[A, B, C1]{ a: a, b: b, c: c } }\n"
  , testGen (construcct (Adt "Some" ["A", "B"] []) (Rule "Rule" [Field "a" "[]int", Field "b" "Opt[T]"])) "func NewRule[A, B any](a []int, b Opt[T]) Some[A, B] { return Rule[A, B]{ a: a, b: b } }\n"
  , testGen (construcct (Adt "Some" ["A"] []) (Rule "Rule" [Field "a" "[]int"])) "func NewRule[A any](a []int) Some[A] { return Rule[A]{ a: a } }\n"
  , testGen (construcct (Adt "Some" ["A"] []) (Rule "Rule" [])) "func NewRule[A any]() Some[A] { return Rule[A]{  } }\n"
  , testGen (construcct (Adt "Some" [] []) (Rule "Rule" [Field "a" "[]int"])) "func NewRule(a []int) Some { return Rule{ a: a } }\n"
  , testGen (construcct (Adt "Some" [] []) (Rule "Rule" [])) "func NewRule() Some { return Rule{  } }\n"

  -- body
  , testGen (body "; " Rule{tag="Add", fields=[Field "a" "Int", Field "b" "[]int"]}) "a Int; b []int"
  , testGen (body " " Rule{tag="Add", fields=[Field "a" "Int", Field "b" "[]int"]}) "a Int b []int"
  , testGen (body "; " Rule{tag="Add", fields=[Field "a" "Int"]}) "a Int"
  , testGen (body "; " Rule{tag="Add", fields=[]}) ""
  ]

main :: IO ()
main = do
    result1 <- runTestTT tests
    if failures result1 > 0 then exitFailure else exitSuccess
