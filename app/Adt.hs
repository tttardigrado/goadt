module Adt where

import Data.List   ( intercalate )
import Text.Printf ( printf      )

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

-- adt interface
-- implName is a dummy function to determine which types implement the interface (data constructors)
-- type Name[A, ... any] interface { implName() }
interface :: Adt -> String
interface a =  printf "type %s%s interface { impl%s() }\n"
  (name a) (gensDef a) (name a)

-- struct for a given data constructor (rule)
-- the body of the struct are the rule's fields
-- type Tag[A ... any] struct { f1 t1; ... }
struct :: Adt -> Rule -> String
struct a r = printf "type %s%s struct { %s }\n"
  (tag r) (gensDef a) (body "; " r)

-- interface implementation for a given data constructor (rule)
-- func (_ Tag[A, ...] implTag() {}
impl :: Adt -> Rule -> String
impl a r = printf "func (_ %s%s) impl%s() {}\n"
  (tag r) (gensFun a) (name a)

-- constructor function for a given data constructor (rule)
-- func NewTag[A, ... any](f1 t1, ...) Name[A, ...] { return Tag[A, ...]{ f1: f1, ... } }
construcct :: Adt -> Rule -> String
construcct a r = printf "func New%s%s(%s) %s%s { return %s%s{ %s } }\n"
  (tag r) (gensDef a) (body ", " r) (name a) (gensFun a) (tag r) (gensFun a) (args $ fields r)
  where args = intercalate ", " . map fset
        fset f = fid f ++ ": " ++ fid f

-- generate the body of a struct, func params, ... based on fields
-- let s = "; " then:
--     id1 ty1; id2 ty2; ... 
body :: String -> Rule -> String
body s = intercalate s . map field . fields
  where field f = fid f ++ " " ++ fty f

-- Golang ADT multiline string generator
-- takes and adt and generates it's
--     interface, structs, impl funcs and constructors 
makeAdt :: Adt -> String
makeAdt a = concat
  [ interface a
  , "\n\n// Data Constructors Structs\n"
  , concatMap (struct a) (rules a)
  , "\n\n// Implement the interface\n"
  , concatMap (impl a) (rules a)
  , "\n\n// Constructors\n"
  , concatMap (construcct a) (rules a)
  ]