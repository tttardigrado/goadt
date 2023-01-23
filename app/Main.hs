{-# LANGUAGE DeriveDataTypeable #-}
module Main where

import System.Console.CmdArgs ( Data, Typeable, (&=), cmdArgs, help, summary )
import Parser                 ( parseAdt )
import Adt                    ( makeAdt, Adt (name) )

data Opts = Opts
  { package :: String
  , adt :: String
  } deriving (Show, Data, Typeable)

opts :: Opts
opts = Opts
  { package = "main" &= help "Package name"
  , adt     = ""     &= help "Adt description"
  } &= summary "Generate a golang ADT"

generate :: String -> String -> IO ()
generate pkg adt = case parseAdt adt of
  Left msg -> print msg
  Right dt ->
    let path = "./" ++ name dt ++ ".go" in
    writeFile path (makeAdt pkg dt)

main :: IO ()
main = do
  op <- cmdArgs opts
  if null $ adt op
    then putStrLn "ERROR: ADT must be provided"
    else generate (package op) (adt op)