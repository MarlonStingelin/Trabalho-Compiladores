module Main where

import Lexer
import Parser

main :: IO ()
main = do
    src <- getContents
    print (parser (alexScanTokens src))