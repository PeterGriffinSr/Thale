#pragma once

#include <stdbool.h>

typedef enum
{
    NodeIntLiteral,
    NodeFloatLiteral,
    NodeStringLiteral,
    NodeCharLiteral,
    NodeBoolLiteral,
    NodeIdentifier,
    NodeBlock,
    NodeBinary,
    NodeUnary,
    NodeVarDecl,
    NodeTypeExpr
} NodeType;

typedef struct ASTNode ASTNode;

struct ASTNode
{
    NodeType type;

    union
    {
        int IntVal;
        double FloatVal;
        const char *StrVal;
        char CharVal;
        bool BoolVal;
    };

    struct
    {
        ASTNode **exprs;
        int cap, count;
    } BlockExpr;

    struct
    {
        const char *op;
        ASTNode *left, *right;
    } BinaryExpr;

    struct
    {
        const char *op;
        ASTNode *left;
    } UnaryExpr;

    struct
    {
        const char *name;
        ASTNode *value, *typeExpr;
    } VarDecl;
};

ASTNode *create_int_node(int value);
ASTNode *create_float_node(double value);
ASTNode *create_string_node(const char *value);
ASTNode *create_char_node(char value);
ASTNode *create_bool_node(bool value);
ASTNode *create_identifier_node(const char *value);
ASTNode *create_block_node(void);
ASTNode *create_binary_node(const char *op, ASTNode *left, ASTNode *right);
ASTNode *create_unary_node(const char *op, ASTNode *left);
ASTNode *append_expr(ASTNode *block, ASTNode *expr);
ASTNode *create_var_decl_node(const char *name, ASTNode *typeExpr, ASTNode *value);
ASTNode *create_type_node(const char *name);

void indent_print(int indentation, const char *fmt, ...);
void printAST(ASTNode *node, int indentation);
void freeAST(ASTNode *node);
