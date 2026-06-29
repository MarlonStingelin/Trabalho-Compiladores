module Gerador where

import AST
import Control.Monad.State

-- ---------------------------------------------------------------------------
-- Estado: contador de labels
-- ---------------------------------------------------------------------------

type Gen a = State Int a

novoLabel :: Gen String
novoLabel = do
    n <- get
    put (n + 1)
    return ("L" ++ show n)

-- ---------------------------------------------------------------------------
-- Tabela local: nome -> (índice slot, tipo)
-- ---------------------------------------------------------------------------

type TabelaLocal = [(Id, (Int, Tipo))]
type TabelaGlobal = [(Id, ([Tipo], Tipo))]

-- ---------------------------------------------------------------------------
-- Descritores de tipo Jasmin
-- ---------------------------------------------------------------------------

descrTipo :: Tipo -> String
descrTipo TInt    = "I"
descrTipo TDouble = "D"
descrTipo TString = "Ljava/lang/String;"
descrTipo TVoid   = "V"

assinatura :: [Tipo] -> Tipo -> String
assinatura params ret =
    "(" ++ concatMap descrTipo params ++ ")" ++ descrTipo ret

-- ---------------------------------------------------------------------------
-- Ponto de entrada
-- ---------------------------------------------------------------------------

gerar :: String -> Programa -> TabelaGlobal -> String
gerar nomeClasse (Prog _ corpos varsMain blocoMain) tblGlobal =
    fst $ runState gen 0
  where
    gen = do
        cab     <- genCabecalho nomeClasse
        metodos <- mapM (genFuncao nomeClasse tblGlobal) corpos
        mainM   <- genMain nomeClasse blocoMain varsMain tblGlobal
        return (cab ++ concat metodos ++ mainM)

-- ---------------------------------------------------------------------------
-- Cabeçalho
-- ---------------------------------------------------------------------------

genCabecalho :: String -> Gen String
genCabecalho nome = return $
    ".class public " ++ nome ++ "\n" ++
    ".super java/lang/Object\n\n" ++
    ".method public <init>()V\n" ++
    "\taload_0\n" ++
    "\tinvokenonvirtual java/lang/Object/<init>()V\n" ++
    "\treturn\n" ++
    ".end method\n\n"

-- ---------------------------------------------------------------------------
-- Método main
-- ---------------------------------------------------------------------------

genMain :: String -> Bloco -> [Var] -> TabelaGlobal -> Gen String
genMain nc bloco vars tg = do
    let tl = buildTL vars
    corpo <- genBloco nc tg tl bloco
    return $
        ".method public static main([Ljava/lang/String;)V\n" ++
        "\t.limit stack 16\n" ++
        "\t.limit locals 16\n\n" ++
        corpo ++
        "\treturn\n" ++
        ".end method\n"

-- ---------------------------------------------------------------------------
-- Helper: constrói TabelaLocal a partir de [Var] com slots sequenciais
-- ---------------------------------------------------------------------------

buildTL :: [Var] -> TabelaLocal
buildTL vars = zipWith mkEntry vars [0..]
  where
    mkEntry (n :#: (t, _)) i = (n, (i, t))

-- ---------------------------------------------------------------------------
-- Geração de função
-- ---------------------------------------------------------------------------

genFuncao :: String -> TabelaGlobal -> (Id, [Var], Bloco) -> Gen String
genFuncao nc tg (nome, params, bloco) = do
    let tbl   = buildTL params
        (tiposP, tipoR) = case lookup nome tg of
                            Just x  -> x
                            Nothing -> (map varTipo params, TVoid)
        assin = assinatura tiposP tipoR
    corpo <- genBloco nc tg tbl bloco
    -- Garante return no fim (necessário se não houver Ret no bloco)
    let retFinal = case tipoR of
                    TVoid   -> "\treturn\n"
                    TDouble -> "\tdconst_0\n\tdreturn\n"
                    _       -> "\ticonst_0\n\tireturn\n"
    return $
        ".method public static " ++ nome ++ assin ++ "\n" ++
        "\t.limit stack 16\n" ++
        "\t.limit locals 16\n\n" ++
        corpo ++
        retFinal ++
        ".end method\n\n"
  where
    varTipo (_ :#: (t, _)) = t

-- ---------------------------------------------------------------------------
-- Geração de bloco
-- ---------------------------------------------------------------------------

genBloco :: String -> TabelaGlobal -> TabelaLocal -> Bloco -> Gen String
genBloco nc tg tl = fmap concat . mapM (genCmd nc tg tl)

-- ---------------------------------------------------------------------------
-- Geração de comandos
-- ---------------------------------------------------------------------------

genCmd :: String -> TabelaGlobal -> TabelaLocal -> Comando -> Gen String

-- Atribuição
genCmd nc tg tl (Atrib nome expr) = do
    (tExpr, cExpr) <- genExpr nc tg tl expr
    let (slot, _) = case lookup nome tl of
                        Just x  -> x
                        Nothing -> (0, TInt)
        store = storeInstr tExpr slot
    return (cExpr ++ store)

-- Impressão
genCmd nc tg tl (Imp expr) = do
    (tExpr, cExpr) <- genExpr nc tg tl expr
    let metodo = case tExpr of
                    TInt    -> "println(I)V"
                    TDouble -> "println(D)V"
                    TString -> "println(Ljava/lang/String;)V"
                    TVoid   -> "println(I)V"
    return $
        "\tgetstatic java/lang/System/out Ljava/io/PrintStream;\n" ++
        cExpr ++
        "\tinvokevirtual java/io/PrintStream/" ++ metodo ++ "\n"

-- Leitura: usa Scanner; o tipo da variável determina nextInt/nextDouble
genCmd nc tg tl (Leitura nome) = do
    let (slot, t) = case lookup nome tl of
                        Just x  -> x
                        Nothing -> (0, TInt)
        (nextM, storeI) = case t of
            TDouble -> ("nextDouble()D", "\tdstore " ++ show slot ++ "\n")
            _       -> ("nextInt()I",    "\tistore " ++ show slot ++ "\n")
    return $
        "\tnew java/util/Scanner\n" ++
        "\tdup\n" ++
        "\tgetstatic java/lang/System/in Ljava/io/InputStream;\n" ++
        "\tinvokespecial java/util/Scanner/<init>(Ljava/io/InputStream;)V\n" ++
        "\tinvokevirtual java/util/Scanner/" ++ nextM ++ "\n" ++
        storeI

-- Retorno void
genCmd _ _ _ (Ret Nothing) = return "\treturn\n"

-- Retorno com valor
genCmd nc tg tl (Ret (Just expr)) = do
    (t, cExpr) <- genExpr nc tg tl expr
    let ret = case t of
                TDouble -> "\tdreturn\n"
                _       -> "\tireturn\n"
    return (cExpr ++ ret)

-- If sem else
genCmd nc tg tl (If exprL b1 []) = do
    lVerd  <- novoLabel
    lFim   <- novoLabel
    cond   <- genExprL nc tg tl lVerd lFim exprL
    corpo1 <- genBloco nc tg tl b1
    return $
        cond ++
        lVerd ++ ":\n" ++
        corpo1 ++
        lFim ++ ":\n"

-- If com else
genCmd nc tg tl (If exprL b1 b2) = do
    lVerd  <- novoLabel
    lFalse <- novoLabel
    lFim   <- novoLabel
    cond   <- genExprL nc tg tl lVerd lFalse exprL
    corpo1 <- genBloco nc tg tl b1
    corpo2 <- genBloco nc tg tl b2
    return $
        cond ++
        lVerd ++ ":\n" ++
        corpo1 ++
        "\tgoto " ++ lFim ++ "\n" ++
        lFalse ++ ":\n" ++
        corpo2 ++
        lFim ++ ":\n"

-- While
genCmd nc tg tl (While exprL bloco) = do
    lInicio <- novoLabel
    lVerd   <- novoLabel
    lFim    <- novoLabel
    cond    <- genExprL nc tg tl lVerd lFim exprL
    corpo   <- genBloco nc tg tl bloco
    return $
        lInicio ++ ":\n" ++
        cond ++
        lVerd ++ ":\n" ++
        corpo ++
        "\tgoto " ++ lInicio ++ "\n" ++
        lFim ++ ":\n"

-- Chamada de procedimento
genCmd nc tg tl (Proc nome args) = do
    cArgs <- genArgList nc tg tl args
    let (tiposF, tipoR) = case lookup nome tg of
                            Just x  -> x
                            Nothing -> ([], TVoid)
        assin = assinatura tiposF tipoR
        pop   = case tipoR of
                    TVoid   -> ""
                    TDouble -> "\tpop2\n"
                    _       -> "\tpop\n"
    return $ cArgs ++
             "\tinvokestatic " ++ nc ++ "/" ++ nome ++ assin ++ "\n" ++
             pop

-- ---------------------------------------------------------------------------
-- Geração de lista de argumentos (push de cada arg em ordem)
-- ---------------------------------------------------------------------------

genArgList :: String -> TabelaGlobal -> TabelaLocal -> [Expr] -> Gen String
genArgList nc tg tl args = do
    rs <- mapM (genExpr nc tg tl) args
    return $ concatMap snd rs

-- ---------------------------------------------------------------------------
-- Instrução de armazenamento conforme o tipo
-- ---------------------------------------------------------------------------

storeInstr :: Tipo -> Int -> String
storeInstr TDouble slot = "\tdstore " ++ show slot ++ "\n"
storeInstr _       slot = "\tistore " ++ show slot ++ "\n"

-- ---------------------------------------------------------------------------
-- Instrução de carregamento conforme o tipo
-- ---------------------------------------------------------------------------

loadInstr :: Tipo -> Int -> String
loadInstr TDouble slot = "\tdload " ++ show slot ++ "\n"
loadInstr _       slot = "\tiload "  ++ show slot ++ "\n"

-- ---------------------------------------------------------------------------
-- Geração de expressões
-- Retorna: (tipo do resultado, código gerado)
-- ---------------------------------------------------------------------------

genExpr :: String -> TabelaGlobal -> TabelaLocal -> Expr -> Gen (Tipo, String)

-- Constante inteira
genExpr _ _ _ (Const (CInt i)) = return (TInt, genInt i)

-- Constante double
genExpr _ _ _ (Const (CDouble d)) = return (TDouble, genDouble d)

-- Literal string
genExpr _ _ _ (Lit s) = return (TString, "\tldc \"" ++ s ++ "\"\n")

-- Variável: lookup na tabela local para saber slot e tipo
genExpr _ _ tl (IdVar nome) = do
    let (slot, t) = case lookup nome tl of
                        Just x  -> x
                        Nothing -> (0, TInt)
    return (t, loadInstr t slot)

-- Negação unária
genExpr nc tg tl (Neg e) = do
    (t, ce) <- genExpr nc tg tl e
    let neg = case t of
                TDouble -> "\tdneg\n"
                _       -> "\tineg\n"
    return (t, ce ++ neg)

-- Conversão int -> double (nó inserido pelo analisador semântico)
genExpr nc tg tl (IntDouble e) = do
    (_, ce) <- genExpr nc tg tl e
    return (TDouble, ce ++ "\ti2d\n")

-- Conversão double -> int (nó inserido pelo analisador semântico)
genExpr nc tg tl (DoubleInt e) = do
    (_, ce) <- genExpr nc tg tl e
    return (TInt, ce ++ "\td2i\n")

-- Operações binárias
genExpr nc tg tl (Add e1 e2) = genBinOp nc tg tl e1 e2 "iadd" "dadd"
genExpr nc tg tl (Sub e1 e2) = genBinOp nc tg tl e1 e2 "isub" "dsub"
genExpr nc tg tl (Mul e1 e2) = genBinOp nc tg tl e1 e2 "imul" "dmul"
genExpr nc tg tl (Div e1 e2) = genBinOp nc tg tl e1 e2 "idiv" "ddiv"

-- Chamada de função como expressão
genExpr nc tg tl (Chamada nome args) = do
    cArgs <- genArgList nc tg tl args
    let (tiposF, tipoR) = case lookup nome tg of
                            Just x  -> x
                            Nothing -> ([], TVoid)
        assin = assinatura tiposF tipoR
    return (tipoR,
            cArgs ++ "\tinvokestatic " ++ nc ++ "/" ++ nome ++ assin ++ "\n")

-- ---------------------------------------------------------------------------
-- Operação binária com promoção de tipo
-- ---------------------------------------------------------------------------

genBinOp :: String -> TabelaGlobal -> TabelaLocal
         -> Expr -> Expr -> String -> String
         -> Gen (Tipo, String)
genBinOp nc tg tl e1 e2 opInt opDouble = do
    (t1, c1) <- genExpr nc tg tl e1
    (t2, c2) <- genExpr nc tg tl e2
    let t  = if t1 == TDouble || t2 == TDouble then TDouble else TInt
        op = if t == TDouble then "\t" ++ opDouble ++ "\n"
                             else "\t" ++ opInt    ++ "\n"
    return (t, c1 ++ c2 ++ op)

-- ---------------------------------------------------------------------------
-- Constantes inteiras: instrução mais compacta possível
-- ---------------------------------------------------------------------------

genInt :: Int -> String
genInt i
    | i == -1            = "\ticonst_m1\n"
    | i >= 0 && i <= 5   = "\ticonst_" ++ show i ++ "\n"
    | i >= -128 && i <= 127  = "\tbipush " ++ show i ++ "\n"
    | i >= -32768 && i <= 32767 = "\tsipush " ++ show i ++ "\n"
    | otherwise          = "\tldc " ++ show i ++ "\n"

-- ---------------------------------------------------------------------------
-- Constantes double
-- ---------------------------------------------------------------------------

genDouble :: Double -> String
genDouble d
    | d == 0.0  = "\tdconst_0\n"
    | d == 1.0  = "\tdconst_1\n"
    | otherwise = "\tldc2_w " ++ show d ++ "\n"

-- ---------------------------------------------------------------------------
-- Geração de expressões lógicas
--
-- Convenção backpatching por labels:
--   se condição VERDADEIRA -> salta para lV
--   se condição FALSA      -> salta para lF
-- ---------------------------------------------------------------------------

genExprL :: String -> TabelaGlobal -> TabelaLocal
         -> String -> String -> ExprL -> Gen String

-- AND: curto-circuito — se e1 falsa vai direto ao lF
genExprL nc tg tl lV lF (And e1 e2) = do
    lMeio <- novoLabel
    c1 <- genExprL nc tg tl lMeio lF e1
    c2 <- genExprL nc tg tl lV   lF e2
    return $ c1 ++ lMeio ++ ":\n" ++ c2

-- OR: curto-circuito — se e1 verdadeira vai direto ao lV
genExprL nc tg tl lV lF (Or e1 e2) = do
    lMeio <- novoLabel
    c1 <- genExprL nc tg tl lV lMeio e1
    c2 <- genExprL nc tg tl lV lF    e2
    return $ c1 ++ lMeio ++ ":\n" ++ c2

-- NOT: troca os labels
genExprL nc tg tl lV lF (Not e) = genExprL nc tg tl lF lV e

-- Relacional: folha
genExprL nc tg tl lV lF (Rel r) = genExprR nc tg tl lV lF r

-- ---------------------------------------------------------------------------
-- Geração de expressões relacionais
-- ---------------------------------------------------------------------------

genExprR :: String -> TabelaGlobal -> TabelaLocal
         -> String -> String -> ExprR -> Gen String

genExprR nc tg tl lV lF (Req  e1 e2) = genRel nc tg tl lV lF e1 e2 "if_icmpeq" "ifeq"
genExprR nc tg tl lV lF (Rdif e1 e2) = genRel nc tg tl lV lF e1 e2 "if_icmpne" "ifne"
genExprR nc tg tl lV lF (Rlt  e1 e2) = genRel nc tg tl lV lF e1 e2 "if_icmplt" "iflt"
genExprR nc tg tl lV lF (Rgt  e1 e2) = genRel nc tg tl lV lF e1 e2 "if_icmpgt" "ifgt"
genExprR nc tg tl lV lF (Rle  e1 e2) = genRel nc tg tl lV lF e1 e2 "if_icmple" "ifle"
genExprR nc tg tl lV lF (Rge  e1 e2) = genRel nc tg tl lV lF e1 e2 "if_icmpge" "ifge"

-- Compara dois operandos e salta:
--   ints  -> if_icmp<op> lV ; goto lF
--   doubles -> dcmpl ; if<op> lV ; goto lF
genRel :: String -> TabelaGlobal -> TabelaLocal
       -> String -> String
       -> Expr -> Expr
       -> String -> String   -- opInt, opDouble
       -> Gen String
genRel nc tg tl lV lF e1 e2 opInt opDbl = do
    (t1, c1) <- genExpr nc tg tl e1
    (t2, c2) <- genExpr nc tg tl e2
    if t1 == TDouble || t2 == TDouble
        then return $
            c1 ++ c2 ++
            "\tdcmpl\n" ++
            "\t" ++ opDbl ++ " " ++ lV ++ "\n" ++
            "\tgoto " ++ lF ++ "\n"
        else return $
            c1 ++ c2 ++
            "\t" ++ opInt ++ " " ++ lV ++ "\n" ++
            "\tgoto " ++ lF ++ "\n"
