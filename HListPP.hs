{-# LANGUAGE QuasiQuotes, ViewPatterns, ApplicativeDo #-}
module Main where

import Data.Char
import Data.List
import Data.Monoid
import System.Environment
import Text.Regex.Applicative
import Control.Exception
import System.Exit
import System.IO

-- "ModuleName."
modNameDot = do
    m <- psym isUpper
    odName <- many (psym isAlpha <|> psym isDigit)
    dot <- sym '.'
    pure $ m:odName ++ [dot]

-- "M.Od.Ule.Name.something"
qualIdent = do
    modNames <- many modNameDot
    end <- identSym <|> identAlpha
    pure $ concat (modNames ++ [end])

identSym = some (psym isSymbol)

identAlpha  = do
  i <- psym isAlpha
  dent <- many (psym isAlpha <|> psym isDigit <|> sym '\'')
  pure $ i:dent

takeQual x = case findLongestPrefix qualIdent x of
    Just (a , '`' : rest) -> ('`' : a ++ "`", rest) -- `infix`
    Just (a, rest) -> (addLabel a,rest)             -- `ident
    Nothing -> ("``", x) -- unlikely

addLabel xu = "(hLens' (Label :: Label \""++xu++"\"))"

main = do
  args <- getArgs
  operate args
    `catch` \ (SomeException e) -> do
          hPutStrLn stderr (show e)
          hPutStrLn stderr ("in: HListPP " ++ unwords args)
          exitFailure

{-# INLINE operate #-}
operate [originalFileName, inputFile, outputFile] = do
    input <- readFile inputFile
    let linePragma = "{-# LINE 1 \"" <> originalFileName <> "\" #-}\n"
    writeFile outputFile (linePragma <> s input)
operate args = error $ "usage: HListPP originalFileName inputFile outputFile\
  \ also: \
  \ {-# OPTIONS_GHC -F -pgmF HListPP #-}\
  \ called with arguments: " ++ show args

-- | applies takeQual outside of characters, strings, comments
{-# INLINE s #-}
s (stripPrefix "'\"'" -> Just xs) = "'\"'" ++ s xs
s (stripPrefix "'`'"  -> Just xs) = "'`'"  ++ s xs
s (stripPrefix "{-" -> Just xs) = "{-" ++ cl xs
s (stripPrefix "--" -> Just xs)
        | x1 : _ <- xs, isSymbol x1 = "--" ++ s xs
        | otherwise = "-- " ++ cs xs
s ('"': xs) = '"' : t xs
s ('`':  (takeQual -> (a,xs))) = a ++ s xs
s (x:xs) = x : s xs
s [] = []

-- | inside string
{-# INLINE t #-}
t (stripPrefix "\\\"" -> Just xs) = "\\\"" ++ t xs
t ('"' : xs) = '"' : s xs
t (x:xs) = x : t xs
t [] = error "expected \""

-- | inside multiline comment
cl (stripPrefix "-}" -> Just xs) = "-}" ++ s xs
cl (x:xs) = x:cl xs
cl [] = error "expected -}"

-- | inside single line comment
cs ('\n':xs) = '\n' : s xs
cs (x:xs) = x : cs xs
cs "" = ""
