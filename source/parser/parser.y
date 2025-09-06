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

%left LogicalOr
%left LogicalAnd
%nonassoc Equal NotEqual
%nonassoc Less LessEqual Greater GreaterEqual
%left Plus Minus
%left Star Slash Percent
%right Bang
%right UMinus

%token LParen RParen LBracket RBracket LBrace RBrace
%token Plus Minus Star Slash Percent Carot 
%token Less Greater Bang Colon Comma Pipe Dot Assignment Underscore Semi
%token Equal NotEqual LessEqual GreaterEqual LogicalAnd LogicalOr Arrow
%token Recursion In Type Use Match True False
%token Int Float String Char Bool Nil Array

%type <node> program expr primary_expr expr_list type param param_list opt_param_list arg_list arg_expr_list decl match_arms pattern

%%

program:
    expr_list { root = $1; };

expr_list:
      decl { $$ = wrap_block_if_needed(create_block_node_append($1)); }
    | expr { $$ = wrap_block_if_needed(create_block_node_append($1)); }
    | expr_list decl { $$ = wrap_block_if_needed(append_expr($1, $2)); }
    | expr_list expr { $$ = wrap_block_if_needed(append_expr($1, $2)); }

decl:
      Identifier LParen opt_param_list RParen Arrow type Assignment expr_list In { ASTNode **params = NULL; int n = 0; if ($3) { params = $3->BlockExpr.exprs; n = $3->BlockExpr.count; } $$ = create_function_node($1, params, n, $6, $8); $$->FunctionDecl.is_recursive = 0; free($3); }
    | Recursion Identifier LParen opt_param_list RParen Arrow type Assignment expr_list In { ASTNode **params = NULL; int n = 0; if ($4) { params = $4->BlockExpr.exprs; n = $4->BlockExpr.count; } $$ = create_function_node($2, params, n, $7, $9); $$->FunctionDecl.is_recursive = 1; free($4); }
    | Identifier LParen RParen Arrow type Assignment expr_list In { $$ = create_function_node($1, NULL, 0, $5, $7); $$->FunctionDecl.is_recursive = 0; }
    | Recursion Identifier LParen RParen Arrow type Assignment expr_list In { $$ = create_function_node($2, NULL, 0, $6, $8); $$->FunctionDecl.is_recursive = 1; };

expr:
      primary_expr { $$ = $1; }
    | Identifier Colon type Assignment expr { $$ = create_var_decl_node($1, $3, $5); }
    | expr Plus expr { $$ = create_binary_node("+", $1, $3); }
    | expr Minus expr { $$ = create_binary_node("-", $1, $3); }
    | expr Star expr { $$ = create_binary_node("*", $1, $3); }
    | expr Slash expr { $$ = create_binary_node("/", $1, $3); }
    | expr Equal expr { $$ = create_binary_node("==", $1, $3); }
    | expr NotEqual expr { $$ = create_binary_node("!=", $1, $3); }
    | expr Less expr { $$ = create_binary_node("<", $1, $3); }
    | expr Greater expr { $$ = create_binary_node(">", $1, $3); }
    | expr LogicalAnd expr { $$ = create_binary_node("&&", $1, $3); }
    | expr LogicalOr expr  { $$ = create_binary_node("||", $1, $3); }
    | Minus expr %prec UMinus { $$ = create_unary_node("-", $2); }
    | Bang expr { $$ = create_unary_node("!", $2); }
    | Match expr LBrace match_arms RBrace { $$ = create_match_node($2, $4); }
    | primary_expr LParen arg_list RParen { ASTNode **args = NULL; int n = 0; if ($3 && $3->BlockExpr.count > 0) { args = $3->BlockExpr.exprs; n = $3->BlockExpr.count; free($3); } else { if ($3) { free($3->BlockExpr.exprs); free($3); } } $$ = create_call_node($1, args, n); }
    | Identifier LParen arg_list RParen { ASTNode **args = NULL; int n = 0; if ($3 && $3->BlockExpr.count > 0) { args = $3->BlockExpr.exprs; n = $3->BlockExpr.count; free($3); } else { if ($3) { free($3->BlockExpr.exprs); free($3); } } $$ = create_call_node(create_identifier_node($1), args, n); };

match_arms:
      Pipe pattern Arrow expr_list { $$ = create_match_arms_node($2, $4, NULL); }
    | match_arms Pipe pattern Arrow expr_list { $$ = append_match_arm($1, $3, $5); };

pattern:
      expr { $$ = $1; }
    | Underscore { $$ = create_wildcard_pattern_node(); };

primary_expr:
      IntLiteral { $$ = create_int_node($1); }
    | FloatLiteral { $$ = create_float_node($1); }
    | CharLiteral { $$ = create_char_node($1); }
    | StringLiteral { $$ = create_string_node($1); }
    | BoolLiteral { $$ = create_bool_node($1); }
    | Identifier { $$ = create_identifier_node($1); }
    | primary_expr Dot Identifier { $$ = create_property_access_node($1, $3); }
    | LBracket arg_expr_list RBracket { ASTNode **elems = NULL; int n = 0; if ($2 && $2->BlockExpr.count > 0) { elems = $2->BlockExpr.exprs; n = $2->BlockExpr.count; free($2); } else { if ($2) { free($2->BlockExpr.exprs); free($2); } } $$ = create_array_node(elems, n); }
    | LParen expr RParen { $$ = $2; };

type:
      Int { $$ = create_type_node("Int"); }
    | Float { $$ = create_type_node("Float"); }
    | String { $$ = create_type_node("String"); }
    | Char { $$ = create_type_node("Char"); }
    | Bool { $$ = create_type_node("Bool"); }
    | Nil { $$ = create_type_node("Nil"); }
    | Array LBracket type RBracket { $$ = create_array_type_node($3); };

arg_list:
      /* empty */ { $$ = create_block_node(); }
    | arg_expr_list { $$ = $1; };

arg_expr_list:
      expr { $$ = create_block_node(); $$ = append_expr($$, $1); }
    | arg_expr_list Comma expr { $$ = append_expr($1, $3); };

opt_param_list:
    param_list { $$ = $1; };

param_list:
      param { $$ = create_block_node(); $$ = append_expr($$, $1); }
    | param_list Comma param { $$ = append_expr($1, $3); };

param:
    Identifier Colon type { $$ = create_param_node($1, $3); };