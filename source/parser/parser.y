%{
    #include <stdio.h>
    #include <string.h>
    #include <stdlib.h>
    #include "ast.h"

    extern FILE *yyin;
    extern ASTNode *root;

    ASTNode *root = NULL;
    int yylex(void);
    void yyerror(const char *s);

    void yyerror(const char *s) {
        fprintf(stderr, "Parse error: %s\n", s);
    }
%}

%union {
    int IntVal;
    double FloatVal;
    const char *StrVal;
    char CharVal;
    _Bool BoolVal;
    struct ASTNode *node;
}

%token <IntVal> IntLiteral
%token <FloatVal> FloatLiteral
%token <StrVal> StringLiteral
%token <CharVal> CharLiteral
%token <BoolVal> BoolLiteral
%token <StrVal> Identifier

%left Plus
%left Star Slash
%right Bang Minus
%left LogicalAnd
%left LogicalOr
%nonassoc Equal NotEqual Less LessEqual Greater GreaterEqual

%token LParen RParen LBracket RBracket LBrace RBrace
%token Plus Minus Star Slash Percent Carot 
%token Less Greater Bang Colon Comma Pipe Dot Assignment Underscore Semi
%token Equal NotEqual LessEqual GreaterEqual LogicalAnd LogicalOr Arrow
%token Recursion Type Use Match True False
%token Int Float String Char Bool Nil Array

%type <node> program expr primary_expr expr_list type

%%

program:
    expr_list { root = $1; };

expr_list:
      expr Semi { $$ = create_block_node(); $$ = append_expr($$, $1); }
    | expr_list expr Semi { $$ = append_expr($1, $2); };

expr:
      Identifier Colon type Assignment expr  { $$ = create_var_decl_node($1, $3, $5); }
    | expr Plus expr     { $$ = create_binary_node("+", $1, $3); }
    | expr Minus expr    { $$ = create_binary_node("-", $1, $3); }
    | expr Star expr     { $$ = create_binary_node("*", $1, $3); }
    | expr Slash expr    { $$ = create_binary_node("/", $1, $3); }
    | expr Equal expr    { $$ = create_binary_node("==", $1, $3); }
    | expr NotEqual expr { $$ = create_binary_node("!=", $1, $3); }
    | expr Less expr     { $$ = create_binary_node("<", $1, $3); }
    | expr Greater expr  { $$ = create_binary_node(">", $1, $3); }
    | expr LogicalAnd expr { $$ = create_binary_node("&&", $1, $3); }
    | expr LogicalOr expr  { $$ = create_binary_node("||", $1, $3); }
    | Minus expr %prec Bang { $$ = create_unary_node("-", $2); }
    | Bang expr { $$ = create_unary_node("!", $2); }
    | primary_expr       { $$ = $1; }
    ;

primary_expr:
      IntLiteral      { $$ = create_int_node($1); }
    | FloatLiteral    { $$ = create_float_node($1); }
    | CharLiteral     { $$ = create_char_node($1); }
    | StringLiteral   { $$ = create_string_node($1); }
    | BoolLiteral     { $$ = create_bool_node($1); }
    | Identifier      { $$ = create_identifier_node($1); }
    | LParen expr RParen { $$ = $2; };

type:
      Int    { $$ = create_type_node("Int"); }
    | Float  { $$ = create_type_node("Float"); }
    | String { $$ = create_type_node("String"); }
    | Char   { $$ = create_type_node("Char"); }
    | Bool   { $$ = create_type_node("Bool"); }
    ;