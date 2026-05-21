{
module Lexer where
import Tokens
}

%wrapper "basic"

$digit = 0-9
$alpha = [a-zA-Z]
$idchar = [$alpha $digit _]

tokens :-

$white+ ;

"int"      { \_ -> TkInt }
"double"   { \_ -> TkDouble }
"string"   { \_ -> TkString }
"void"     { \_ -> TkVoid }

"if"       { \_ -> TkIf }
"else"     { \_ -> TkElse }
"while"    { \_ -> TkWhile }
"return"   { \_ -> TkReturn }
"print"    { \_ -> TkPrint }
"read"     { \_ -> TkRead }

"+"        { \_ -> TkPlus }
"-"        { \_ -> TkMinus }
"*"        { \_ -> TkMul }
"/"        { \_ -> TkDiv }

"="        { \_ -> TkAssign }

"=="       { \_ -> TkEq }
"/="       { \_ -> TkDif }
"<="       { \_ -> TkLe }
">="       { \_ -> TkGe }
"<"        { \_ -> TkLt }
">"        { \_ -> TkGt }

"&&"       { \_ -> TkAnd }
"||"       { \_ -> TkOr }
"!"        { \_ -> TkNot }

"("        { \_ -> TkLParen }
")"        { \_ -> TkRParen }
"{"        { \_ -> TkLBrace }
"}"        { \_ -> TkRBrace }

","        { \_ -> TkComma }
";"        { \_ -> TkSemi }

\"[^\"]*\" { \s -> TkStringLit (tail (init s)) }

$digit+"."$digit+ { \s -> TkDoubleConst (read s) }

$digit+ { \s -> TkIntConst (read s) }

$alpha$idchar* { \s -> TkId s }