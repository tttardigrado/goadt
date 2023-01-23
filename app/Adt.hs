module Adt where

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