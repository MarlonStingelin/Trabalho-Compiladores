{
module Parser where

import Tokens
import AST
}

%name parser Programa
%tokentype { Token }
%error { parseError }

%token
    int             { TkInt }
    double          { TkDouble }
    string          { TkString }
    void            { TkVoid }

    if              { TkIf }
    else            { TkElse }
    while           { TkWhile }
    return          { TkReturn }
    print           { TkPrint }
    read            { TkRead }

    '+'             { TkPlus }
    '-'             { TkMinus }
    '*'             { TkMul }
    '/'             { TkDiv }

    '='             { TkAssign }

    '<'             { TkLt }
    '>'             { TkGt }

    le              { TkLe }
    ge              { TkGe }
    eq              { TkEq }
    dif             { TkDif }

    and             { TkAnd }
    or              { TkOr }

    '!'             { TkNot }

    '('             { TkLParen }
    ')'             { TkRParen }

    '{'             { TkLBrace }
    '}'             { TkRBrace }

    ','             { TkComma }
    ';'             { TkSemi }

    id              { TkId $$ }
    inteiro         { TkIntConst $$ }
    real            { TkDoubleConst $$ }
    literal         { TkStringLit $$ }

%left or
%left and
%nonassoc '<' '>' le ge eq dif
%left '+' '-'
%left '*' '/'
%right '!'
%right UMINUS

%%

Programa
    : ListaFuncoes BlocoPrincipal
        { Prog [] $1 [] $2 }
    | BlocoPrincipal
        { Prog [] [] [] $1 }

ListaFuncoes
    : ListaFuncoes Funcao
        { $1 ++ [$2] }
    | Funcao
        { [$1] }

Funcao
    : TipoRet id '(' ParamFormais ')' BlocoPrincipal
        { ($2, $4, $6) }
    | TipoRet id '(' ')' BlocoPrincipal
        { ($2, [], $5) }

TipoRet
    : Tipo
        { $1 }
    | void
        { TVoid }

Tipo
    : int
        { TInt }
    | double
        { TDouble }
    | string
        { TString }

ParamFormais
    : ParamFormais ',' ParamFormal
        { $1 ++ [$3] }
    | ParamFormal
        { [$1] }

ParamFormal
    : Tipo id
        { $2 :#: ($1,0) }

BlocoPrincipal
    : '{' Declaracoes ListaCmd '}'
        { $3 }
    | '{' ListaCmd '}'
        { $2 }

Declaracoes
    : Declaracoes Declaracao
        { $1 ++ $2 }
    | Declaracao
        { $1 }

Declaracao
    : Tipo ListaId ';'
        { map (\x -> x :#: ($1,0)) $2 }

ListaId
    : ListaId ',' id
        { $1 ++ [$3] }
    | id
        { [$1] }

Bloco
    : '{' ListaCmd '}'
        { $2 }

ListaCmd
    : ListaCmd Comando
        { $1 ++ [$2] }
    | Comando
        { [$1] }

Comando
    : CmdSe
        { $1 }
    | CmdEnquanto
        { $1 }
    | CmdAtrib
        { $1 }
    | CmdEscrita
        { $1 }
    | CmdLeitura
        { $1 }
    | Retorno
        { $1 }
    | ChamadaProc
        { $1 }

CmdSe
    : if '(' ExpressaoLogica ')' Bloco
        { If $3 $5 [] }
    | if '(' ExpressaoLogica ')' Bloco else Bloco
        { If $3 $5 $7 }

CmdEnquanto
    : while '(' ExpressaoLogica ')' Bloco
        { While $3 $5 }

CmdAtrib
    : id '=' ExpressaoAritmetica ';'
        { Atrib $1 $3 }
    | id '=' literal ';'
        { Atrib $1 (Lit $3) }

CmdEscrita
    : print '(' ExpressaoAritmetica ')' ';'
        { Imp $3 }
    | print '(' literal ')' ';'
        { Imp (Lit $3) }

CmdLeitura
    : read '(' id ')' ';'
        { Leitura $3 }

Retorno
    : return ExpressaoAritmetica ';'
        { Ret (Just $2) }
    | return literal ';'
        { Ret (Just (Lit $2)) }
    | return ';'
        { Ret Nothing }

ChamadaProc
    : ChamadaFuncao ';'
        { $1 }

ChamadaFuncao
    : id '(' ParamReais ')'
        { Proc $1 $3 }
    | id '(' ')'
        { Proc $1 [] }

ParamReais
    : ParamReais ',' ExpressaoAritmetica
        { $1 ++ [$3] }
    | ParamReais ',' literal
        { $1 ++ [Lit $3] }
    | ExpressaoAritmetica
        { [$1] }
    | literal
        { [Lit $1] }

ExpressaoLogica
    : ExpressaoLogica and ExpressaoLogica
        { And $1 $3 }
    | ExpressaoLogica or ExpressaoLogica
        { Or $1 $3 }
    | '!' ExpressaoLogica
        { Not $2 }
    | ExpressaoRelacional
        { Rel $1 }
    | '(' ExpressaoLogica ')'
        { $2 }

ExpressaoRelacional
    : ExpressaoAritmetica '<' ExpressaoAritmetica
        { Rlt $1 $3 }
    | ExpressaoAritmetica '>' ExpressaoAritmetica
        { Rgt $1 $3 }
    | ExpressaoAritmetica le ExpressaoAritmetica
        { Rle $1 $3 }
    | ExpressaoAritmetica ge ExpressaoAritmetica
        { Rge $1 $3 }
    | ExpressaoAritmetica eq ExpressaoAritmetica
        { Req $1 $3 }
    | ExpressaoAritmetica dif ExpressaoAritmetica
        { Rdif $1 $3 }

ExpressaoAritmetica
    : ExpressaoAritmetica '+' ExpressaoAritmetica
        { Add $1 $3 }
    | ExpressaoAritmetica '-' ExpressaoAritmetica
        { Sub $1 $3 }
    | ExpressaoAritmetica '*' ExpressaoAritmetica
        { Mul $1 $3 }
    | ExpressaoAritmetica '/' ExpressaoAritmetica
        { Div $1 $3 }
    | '-' ExpressaoAritmetica %prec UMINUS
        { Neg $2 }
    | inteiro
        { Const (CInt $1) }
    | real
        { Const (CDouble $1) }
    | id
        { IdVar $1 }
    | id '(' ParamReais ')'
        { Chamada $1 $3 }
    | id '(' ')'
        { Chamada $1 [] }
    | '(' ExpressaoAritmetica ')'
        { $2 }

{
parseError :: [Token] -> a
parseError tokens =
    error ("Erro sintático próximo aos tokens: " ++ show (take 5 tokens))
}