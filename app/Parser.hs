module Parser where

import Adt (Adt (..), Field (..), Rule (..))
import Text.ParserCombinators.Parsec
import Data.List (intercalate)

-- Examples have white spaces, but they are just for readability.
-- the parse function removes them before applying the parsers

-- parser for an identifier
-- starts with a letter and can contain letters, digits and underscore
idP :: Parser String
idP = do
    fchr <- letter
    rest <- many $ letter <|> digit <|> char '_'
    return (fchr:rest)

-- type parser
-- ex: A    []int    []A    []Option[T]    []Option2[T, R]
-- TODO: improve type parser
typeP :: Parser String
typeP = many1 $ noneOf "|();"

-- parse one or more occurencies of `p`'s separated by `s` between `o` and `c`
-- o: open; c: close; s: separator; p: parser
-- ex: between '[' ']' ';' idP
--      parses [a; b_c; d12]
betweenSep :: Char -> Char -> Char -> Parser a -> Parser [a]
betweenSep o c s p = between (char o) (char c) $ p `sepBy` char s

-- parse a generic params list.
-- ex: [a; b_c; d12]
genP :: Parser [String]
genP = betweenSep '[' ']' ';' idP

-- parse a rule's field list.
-- ex: (a: A; b: []B1; c1_2: C[int])
fieldsP :: Parser [Field]
fieldsP = betweenSep '(' ')' ';' fieldP

-- parse a single field
-- ex: a1: A[int]    ab_c: []A    a23: A
fieldP :: Parser Field
fieldP = do
    fid <- idP
    char ':'
    fty <- typeP
    return Field {fid=fid, fty=fty}

-- parse a single rule
-- ex: None()    Some(v: A)    Some2(v1: A; v2: B)
ruleP :: Parser Rule
ruleP = do
    tag <- idP
    fields <- fieldsP
    return Rule {tag=tag, fields=fields}

-- parse a full definition of an ADT
-- ex: Option2[A; B] = Some2(v1: A; v2: B) | Some(v: A) | None()
adtP :: Parser Adt
adtP = do
    name <- idP
    params <- genP
    char '='
    rule <- ruleP `sepBy` char '|'
    return Adt {name=name, params=params, rules=rule}

-- final parsing function
-- removes spaces before running the adtP parser
parseAdt :: String -> Either ParseError Adt
parseAdt = parse adtP "" . filter (/=' ')