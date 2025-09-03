%option noyywrap noinput nounput

%{
#ifdef _WIN32
    #define YY_NO_UNISTD_H
#endif
#include <stdio.h>
#include <string.h>
#include <stdbool.h>
#include "parser.yy.h"

int yycolumn = 1;
%}

Digit               [0-9]
Letter              [a-zA-Z]
Identifier          {Letter}({Letter}|{Digit}|_)*
IntLiteral          {Digit}+
FloatLiteral        ({Digit}+"."{Digit}*|{Digit}*"."{Digit}+)
CharLiteral         \'(\\.|[^\\'])\'
StringLiteral       \"(\\.|[^\\"])*\"

%%

"#"[^\n]*\n         { /* ignore single line comments */ }

"("                 { yycolumn += yyleng; return LParen; }
")"                 { yycolumn += yyleng; return RParen; }
"["                 { yycolumn += yyleng; return LBracket; }
"]"                 { yycolumn += yyleng; return RBracket; }
"{"                 { yycolumn += yyleng; return LBrace; }
"}"                 { yycolumn += yyleng; return RBrace; }
"+"                 { yycolumn += yyleng; return Plus; }
"-"                 { yycolumn += yyleng; return Minus; }
"*"                 { yycolumn += yyleng; return Star; }
"/"                 { yycolumn += yyleng; return Slash; }
"%"                 { yycolumn += yyleng; return Percent; }
"^"                 { yycolumn += yyleng; return Carot; }
"<"                 { yycolumn += yyleng; return Less; }
">"                 { yycolumn += yyleng; return Greater; }
"!"                 { yycolumn += yyleng; return Bang; }
":"                 { yycolumn += yyleng; return Colon; }
","                 { yycolumn += yyleng; return Comma; }
"|"                 { yycolumn += yyleng; return Pipe; }
"."                 { yycolumn += yyleng; return Dot; }
"="                 { yycolumn += yyleng; return Assignment; }
"_"                 { yycolumn += yyleng; return Underscore; }
";"                 { yycolumn += yyleng; return Semi; }
"=="                { yycolumn += yyleng; return Equal; }
"<>"                { yycolumn += yyleng; return NotEqual; }
"<="                { yycolumn += yyleng; return LessEqual; }
">="                { yycolumn += yyleng; return GreaterEqual; }
"||"                { yycolumn += yyleng; return LogicalOr; }
"&&"                { yycolumn += yyleng; return LogicalAnd; }
"->"                { yycolumn += yyleng; return Arrow; }

"rec"               { yycolumn += yyleng; return Recursion; }
"type"              { yycolumn += yyleng; return Type; }
"use"               { yycolumn += yyleng; return Use; }
"match"             { yycolumn += yyleng; return Match; }
"True"              { yycolumn += yyleng; yylval.BoolVal = true; return BoolLiteral; }
"False"             { yycolumn += yyleng; yylval.BoolVal = false; return BoolLiteral; }

"Int"               { yycolumn += yyleng; return Int; }
"Float"             { yycolumn += yyleng; return Float; }
"String"            { yycolumn += yyleng; return String; }
"Char"              { yycolumn += yyleng; return Char; }
"Bool"              { yycolumn += yyleng; return Bool; }
"Nil"               { yycolumn += yyleng; return Nil; }
"Array"             { yycolumn += yyleng; return Array; }

{CharLiteral}       { yycolumn += yyleng; yylval.CharVal = yytext[1]; return CharLiteral; }
{StringLiteral}     { yycolumn += yyleng; yylval.StrVal = yytext; return StringLiteral; }
{IntLiteral}        { yycolumn += yyleng; yylval.IntVal = atoi(yytext); return IntLiteral; }
{Identifier}        { yycolumn += yyleng; yylval.StrVal = strdup(yytext); return Identifier; }

[ \t\r]+            { yycolumn += yyleng; }
\n                  { yycolumn = 1; yylineno++; }

%%