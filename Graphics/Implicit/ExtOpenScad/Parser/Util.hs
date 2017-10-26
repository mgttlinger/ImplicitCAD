-- Implicit CAD. Copyright (C) 2011, Christopher Olah (chris@colah.ca)
-- Copyright (C) 2016 Julia Longtin (julial@turinglace.com)
-- Copyright 2017 Merlin Göttlinger (megoettlinger@gmail.com)
-- Released under the GNU AGPLV3+, see LICENSE

{-# LANGUAGE FlexibleContexts, Rank2Types #-}

module Graphics.Implicit.ExtOpenScad.Parser.Util (Parser, angles, braces, comma, commaSep, curly, equals, genBetween, genSpace, pad, squares, (*<|>), (?:), stringGS, padString, tryMany, variableSymb, patternMatcher) where

import Prelude (String, Char, ($), (++), foldl1, pure, (*>), (<$>), (.))

import Text.ParserCombinators.Parsec (GenParser, anyChar, between, many, oneOf, noneOf, (<|>), try, string, manyTill, (<?>), char, many1, sepBy)

import Text.Parsec.Prim (ParsecT, Stream)
import Data.Functor.Identity (Identity)
import Graphics.Implicit.ExtOpenScad.Definitions (Pattern(Wild, Name, ListP))


type Parser t = forall st. GenParser Char st t

-- white space, including tabs, newlines and comments
genSpace :: Parser [Char]
genSpace = many $
    oneOf " \t\n\r"
    <|> (try $ string "//" *> many ( noneOf "\n") *> string "\n" *> pure ' ')
    <|> (try $ string "/*" *> manyTill anyChar (try $ string "*/") *> pure ' ')

pad :: Parser b -> Parser b
pad = between genSpace genSpace


infixr 1 *<|>
(*<|>) :: GenParser tok u a -> ParsecT [tok] u Identity a -> ParsecT [tok] u Identity a
a *<|> b = try a <|> b

(?:) :: String -> ParsecT s u m a -> ParsecT s u m a
l ?: p = p <?> l

stringGS :: [Char] -> Parser String
stringGS (' ':xs) = do
    x'  <- genSpace
    xs' <- stringGS xs
    pure (x' ++ xs')
stringGS (x:xs) = do
    x'  <- char x
    xs' <- stringGS xs
    pure (x' : xs')
stringGS "" = pure ""

comma :: Parser String
comma = stringGS " , "

commaSep :: Parser t -> Parser [t]
commaSep p = sepBy p comma 

equals :: Parser String
equals = stringGS " = "

genBetween :: String -> String -> Parser t -> Parser t
genBetween b a = between (stringGS b) (stringGS a)

padString :: String -> Parser String
padString s = pad $ string s

squares :: Parser t -> Parser t
squares = genBetween " [ " " ] "

curly :: Parser t -> Parser t
curly = genBetween " { " " } "

angles :: Parser t -> Parser t
angles = genBetween " < " " > "

braces :: Parser t -> Parser t
braces = genBetween " ( " " ) "

tryMany :: [GenParser tok u a] -> ParsecT [tok] u Identity a
tryMany = (foldl1 (<|>)) . (try <$>)

variableSymb :: Stream s m Char => ParsecT s u m [Char]
variableSymb = many1 (noneOf " ,|[]{}()+-*&^%#@!~`'\"\\/;:.,<>?=") <?> "variable"

patternMatcher :: Parser Pattern
patternMatcher =
    (char '_' *> pure Wild)
    <|> {-( do
        a <- literal
        return $ \obj ->
            if obj == (a undefined)
            then Just (Map.empty)
            else Nothing
    ) <|> -} (Name <$> variableSymb)
    <|> (ListP <$> (squares $ pad $ patternMatcher `sepBy` (try $ genSpace *> char ',' *> genSpace)))
