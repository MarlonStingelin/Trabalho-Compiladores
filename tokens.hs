module Tokens where

data Token
    = TkInt
    | TkDouble
    | TkString
    | TkVoid

    | TkIf
    | TkElse
    | TkWhile
    | TkReturn
    | TkPrint
    | TkRead

    | TkPlus
    | TkMinus
    | TkMul
    | TkDiv

    | TkAssign

    | TkEq
    | TkDif
    | TkLt
    | TkGt
    | TkLe
    | TkGe

    | TkAnd
    | TkOr
    | TkNot

    | TkLParen
    | TkRParen
    | TkLBrace
    | TkRBrace

    | TkComma
    | TkSemi

    | TkId String
    | TkIntConst Int
    | TkDoubleConst Double
    | TkStringLit String

    deriving (Show, Eq)