module Semantico where

import AST

data Result a = Result (Bool, String, a) deriving Show

instance Functor Result where
    fmap f (Result (b, s, a)) = Result (b, s, f a)

instance Applicative Result where
    pure a = Result (False, "", a)
    Result (b1, s1, f) <*> Result (b2, s2, x) = Result (b1 || b2, s1 <> s2, f x)

instance Monad Result where
    Result (b, s, a) >>= f =
        let Result (b', s', a') = f a
        in Result (b || b', s ++ s', a')

errorMsg :: String -> Result ()
errorMsg s = Result (True, "Erro: " ++ s ++ "\n", ())

warningMsg :: String -> Result ()
warningMsg s = Result (False, "Advertencia: " ++ s ++ "\n", ())

type TabelaGlobal = [(Id, ([Tipo], Tipo))]

type TabelaLocal = [(Id, Tipo)]

buildTabelaGlobal :: [Funcao] -> Result TabelaGlobal
buildTabelaGlobal = foldl step (pure [])
  where
    step acc (nome :->: (params, ret)) = do
        tbl <- acc
        case lookup nome tbl of
            Just _ -> do
                errorMsg ("Funcao '" ++ nome ++ "' declarada multiplas vezes.")
                return tbl
            Nothing ->
                return (tbl ++ [(nome, (map varTipo params, ret))])
    varTipo (_ :#: (t, _)) = t

buildTabelaLocal :: [Var] -> Result TabelaLocal
buildTabelaLocal = foldl step (pure [])
  where
    step acc (nome :#: (t, _)) = do
        tbl <- acc
        case lookup nome tbl of
            Just _ -> do
                errorMsg ("Variavel '" ++ nome ++ "' declarada multiplas vezes.")
                return tbl
            Nothing ->
                return (tbl ++ [(nome, t)])

analisaPrograma :: Programa -> Result Programa
analisaPrograma (Prog funcoes corpos varsGlobais blocoMain) = do
    tblGlobal <- buildTabelaGlobal funcoes

    corposAnalisados <- mapM (analisaCorpo tblGlobal) corpos

    tblMain <- buildTabelaLocal varsGlobais
    blocoMainAnalisado <- analisaBloco tblGlobal tblMain TVoid blocoMain

    return (Prog funcoes corposAnalisados varsGlobais blocoMainAnalisado)

analisaCorpo :: TabelaGlobal
             -> (Id, [Var], Bloco)
             -> Result (Id, [Var], Bloco)
analisaCorpo tblGlobal (nome, params, bloco) = do
    tblLocal <- buildTabelaLocal params
    let (_, tipoRet) = case lookup nome tblGlobal of
                         Just x  -> x
                         Nothing -> ([], TVoid)
    blocoAnalisado <- analisaBloco tblGlobal tblLocal tipoRet bloco
    return (nome, params, blocoAnalisado)

analisaBloco :: TabelaGlobal -> TabelaLocal -> Tipo -> Bloco -> Result Bloco
analisaBloco tg tl ret = mapM (analisaCmd tg tl ret)

analisaCmd :: TabelaGlobal -> TabelaLocal -> Tipo -> Comando -> Result Comando
analisaCmd tg tl ret cmd = case cmd of

    Atrib nome expr -> do
        tipoVar <- lookupVar nome tl
        (exprA, tipoExpr) <- analisaExpr tg tl expr
        exprFinal <- coerceAtrib tipoVar tipoExpr exprA
                       ("na atribuicao a variavel '" ++ nome ++ "'")
        return (Atrib nome exprFinal)

    Imp expr -> do
        (exprA, _) <- analisaExpr tg tl expr
        return (Imp exprA)

    Leitura nome -> do
        _ <- lookupVar nome tl
        return (Leitura nome)

    Ret Nothing -> return (Ret Nothing)
    Ret (Just expr) -> do
        (exprA, tipoExpr) <- analisaExpr tg tl expr
        exprFinal <- coerceAtrib ret tipoExpr exprA
                       ("no retorno da funcao (esperado " ++ show ret ++ ")")
        return (Ret (Just exprFinal))

    If exprL b1 b2 -> do
        exprLA <- analisaExprL tg tl exprL
        b1A    <- analisaBloco tg tl ret b1
        b2A    <- analisaBloco tg tl ret b2
        return (If exprLA b1A b2A)

    While exprL bloco -> do
        exprLA  <- analisaExprL tg tl exprL
        blocoA  <- analisaBloco tg tl ret bloco
        return (While exprLA blocoA)

    Proc nome args -> do
        argsA <- analisaChamada tg tl nome args
        return (Proc nome argsA)

lookupVar :: Id -> TabelaLocal -> Result Tipo
lookupVar nome tl =
    case lookup nome tl of
        Just t  -> return t
        Nothing -> do
            errorMsg ("Variavel '" ++ nome ++ "' nao declarada.")
            return TInt  

analisaExpr :: TabelaGlobal -> TabelaLocal -> Expr -> Result (Expr, Tipo)
analisaExpr tg tl expr = case expr of

    Const (CInt _)    -> return (expr, TInt)
    Const (CDouble _) -> return (expr, TDouble)
    Lit _             -> return (expr, TString)

    IdVar nome -> do
        t <- lookupVar nome tl
        return (IdVar nome, t)

    Neg e -> do
        (eA, t) <- analisaExpr tg tl e
        case t of
            TInt    -> return (Neg eA, TInt)
            TDouble -> return (Neg eA, TDouble)
            _       -> do
                errorMsg ("Operador unario '-' aplicado a tipo incompativel: " ++ show t)
                return (Neg eA, TInt)

    Add e1 e2 -> analisaBinArit tg tl Add e1 e2 "+"
    Sub e1 e2 -> analisaBinArit tg tl Sub e1 e2 "-"
    Mul e1 e2 -> analisaBinArit tg tl Mul e1 e2 "*"
    Div e1 e2 -> analisaBinArit tg tl Div e1 e2 "/"

    Chamada nome args -> do
        argsA   <- analisaChamada tg tl nome args
        tipoRet <- case lookup nome tg of
                     Just (_, r) -> return r
                     Nothing     -> return TInt
        return (Chamada nome argsA, tipoRet)

    IntDouble e -> do
        (eA, _) <- analisaExpr tg tl e
        return (IntDouble eA, TDouble)
    DoubleInt e -> do
        (eA, _) <- analisaExpr tg tl e
        return (DoubleInt eA, TInt)

analisaBinArit :: TabelaGlobal -> TabelaLocal
               -> (Expr -> Expr -> Expr)
               -> Expr -> Expr -> String
               -> Result (Expr, Tipo)
analisaBinArit tg tl cons e1 e2 op = do
    (e1A, t1) <- analisaExpr tg tl e1
    (e2A, t2) <- analisaExpr tg tl e2
    case (t1, t2) of
        (TInt,    TInt)    -> return (cons e1A e2A, TInt)
        (TDouble, TDouble) -> return (cons e1A e2A, TDouble)
        (TInt,    TDouble) -> return (cons (IntDouble e1A) e2A, TDouble)
        (TDouble, TInt)    -> return (cons e1A (IntDouble e2A), TDouble)
        _ -> do
            errorMsg ("Tipos incompativeis no operador '" ++ op
                      ++ "': " ++ show t1 ++ " e " ++ show t2)
            return (cons e1A e2A, TInt)

--expressões relacionais
analisaExprR :: TabelaGlobal -> TabelaLocal -> ExprR -> Result ExprR
analisaExprR tg tl exprR = case exprR of
    Req e1 e2  -> analisaBinRel tg tl Req  e1 e2 "=="
    Rdif e1 e2 -> analisaBinRel tg tl Rdif e1 e2 "/="
    Rlt e1 e2  -> analisaBinRel tg tl Rlt  e1 e2 "<"
    Rgt e1 e2  -> analisaBinRel tg tl Rgt  e1 e2 ">"
    Rle e1 e2  -> analisaBinRel tg tl Rle  e1 e2 "<="
    Rge e1 e2  -> analisaBinRel tg tl Rge  e1 e2 ">="

analisaBinRel :: TabelaGlobal -> TabelaLocal
              -> (Expr -> Expr -> ExprR)
              -> Expr -> Expr -> String
              -> Result ExprR
analisaBinRel tg tl cons e1 e2 op = do
    (e1A, t1) <- analisaExpr tg tl e1
    (e2A, t2) <- analisaExpr tg tl e2
    case (t1, t2) of

        (TString, TString) -> return (cons e1A e2A)

        (TString, _) -> do
            errorMsg ("Operador '" ++ op ++ "': string nao pode ser comparado com "
                      ++ show t2)
            return (cons e1A e2A)
        (_, TString) -> do
            errorMsg ("Operador '" ++ op ++ "': string nao pode ser comparado com "
                      ++ show t1)
            return (cons e1A e2A)

        (TInt,    TInt)    -> return (cons e1A e2A)
        (TDouble, TDouble) -> return (cons e1A e2A)
        (TInt,    TDouble) -> return (cons (IntDouble e1A) e2A)
        (TDouble, TInt)    -> return (cons e1A (IntDouble e2A))
        _ -> do
            errorMsg ("Tipos incompativeis no operador '" ++ op
                      ++ "': " ++ show t1 ++ " e " ++ show t2)
            return (cons e1A e2A)

--expressões lógicas
analisaExprL :: TabelaGlobal -> TabelaLocal -> ExprL -> Result ExprL
analisaExprL tg tl exprL = case exprL of
    And l1 l2 -> do
        l1A <- analisaExprL tg tl l1
        l2A <- analisaExprL tg tl l2
        return (And l1A l2A)
    Or l1 l2 -> do
        l1A <- analisaExprL tg tl l1
        l2A <- analisaExprL tg tl l2
        return (Or l1A l2A)
    Not l -> do
        lA <- analisaExprL tg tl l
        return (Not lA)
    Rel r -> do
        rA <- analisaExprR tg tl r
        return (Rel rA)

--chamadas de função
analisaChamada :: TabelaGlobal -> TabelaLocal -> Id -> [Expr] -> Result [Expr]
analisaChamada tg tl nome args =
    case lookup nome tg of
        Nothing -> do
            errorMsg ("Funcao '" ++ nome ++ "' nao declarada.")
            mapM (\a -> fst <$> analisaExpr tg tl a) args
        Just (tiposFormais, _) -> do
            argsA <- mapM (\a -> analisaExpr tg tl a) args
            let nArgs    = length args
                nFormais = length tiposFormais
            if nArgs /= nFormais
              then do
                errorMsg ("Funcao '" ++ nome ++ "': numero de parametros incorreto."
                          ++ " Esperado " ++ show nFormais
                          ++ ", recebido " ++ show nArgs ++ ".")
                return (map fst argsA)
              else do
                let triplas = zip3 tiposFormais (map fst argsA) (map snd argsA)
                mapM (\(tf, eA, ta) ->
                        coerceAtrib tf ta eA
                          ("em parametro de '" ++ nome ++ "'"
                           ++ " (esperado " ++ show tf
                           ++ ", recebido " ++ show ta ++ ")"))
                     triplas

--tipos atribuição
coerceAtrib :: Tipo -> Tipo -> Expr -> String -> Result Expr
coerceAtrib dest src expr ctx = case (dest, src) of
    (TInt,    TInt)    -> return expr
    (TDouble, TDouble) -> return expr
    (TString, TString) -> return expr
    (TVoid,   _)       -> return expr
    (TDouble, TInt)    -> return (IntDouble expr)
    (TInt,    TDouble) -> do
        warningMsg ("Conversao de double para int " ++ ctx ++ ".")
        return (DoubleInt expr)
    _ -> do
        errorMsg ("Tipo incompativel " ++ ctx
                  ++ ": esperado " ++ show dest
                  ++ ", recebido " ++ show src ++ ".")
        return expr

analisaSemântica :: Programa -> (Bool, String, Programa)
analisaSemântica prog =
    let Result (err, msgs, progA) = analisaPrograma prog
    in (err, msgs, progA)
