module Adt where

import Data.List (intercalate)

-- data name[p1, p2, ...] = r1 | r2 | ...
data Adt = Adt
  { name   :: String   -- adt name
  , params :: [String] -- generic type parameters
  , rules  :: [Rule]   -- data construtors
  } deriving Show

-- tag(f1, f2, ...)
data Rule = Rule
  { tag    :: String  -- data constructor tag
  , fields :: [Field] -- data constructor arguments
  } deriving Show

-- id : type
data Field = Field
  { fid :: String -- field id
  , fty :: String -- field type
  } deriving Show


-- generic types to be used in type and function definitions
-- if no generic type is needed returns an empty string
-- [A, B, C, ... any]
gensDef :: Adt -> String
gensDef a = if null $ params a then "" else printf "[%s any]"
  (intercalate ", " $ params a)

-- generic types to be used in function calls and type creation
-- if no generic type is needed returns an empty string
-- [A, B, C, ...]
gensFun :: Adt -> String
gensFun a = if null $ params a then "" else printf "[%s]"
  (intercalate ", " $ params a)