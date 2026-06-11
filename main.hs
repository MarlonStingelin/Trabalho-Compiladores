module Main where

import Lexer
import Parser
import Semantico

main :: IO ()
main = do
    src <- getContents
    let ast = parser (alexScanTokens src)
    let (temErro, msgs, astAnotada) = analisaSemântica ast
    if null msgs
        then putStrLn "Analise semantica concluida sem erros ou advertencias."
        else putStr msgs
    if temErro
        then putStrLn "Analise semantica concluida sem erros ou advertencias."
        else print astAnotada
