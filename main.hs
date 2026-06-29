module Main where

import Lexer
import Parser
import Semantico
import Gerador
import AST
import System.IO (hPutStrLn, stderr)

main :: IO ()
main = do
    src <- getContents
    let nomeClasse = "Programa"

    let ast = parser (alexScanTokens src)

    let (temErro, msgs, astAnotada) = analisaSemântica ast

    if null msgs
        then return ()
        else hPutStrLn stderr msgs

    if temErro
        then hPutStrLn stderr "Compilacao abortada: corrija os erros semanticos."
        else do
            let tblGlobal = buildTblGlobal astAnotada
            let jasmin = gerar nomeClasse astAnotada tblGlobal
            putStr jasmin

buildTblGlobal :: Programa -> [(Id, ([Tipo], Tipo))]
buildTblGlobal (Prog funcoes _ _ _) =
    map entry funcoes
  where
    entry (nome :->: (params, ret)) = (nome, (map varTipo params, ret))
    varTipo (_ :#: (t, _)) = t
