module Main where


import Test.HUnit
import System.Exit ( exitSuccess, exitFailure )


tests :: Test
tests = TestList
  [
  ]

main :: IO ()
main = do
    result1 <- runTestTT tests
    if failures result1 > 0 then exitFailure else exitSuccess