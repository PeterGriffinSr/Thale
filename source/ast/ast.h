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
    NodeTypeExpr,
    NodeFunctionDecl,
    NodeParam,
    NodeCallExpr,
    NodeArrayLiteral,
    NodeArrayType,
    NodePropertyAccess,
    NodeMatch,
    NodeMatchArm
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

    struct
    {
        const char *name;
        ASTNode **params, *returnType, *body;
        int param_count;
        bool is_recursive;
    } FunctionDecl;

    struct
    {
        const char *name;
        ASTNode *typeExpr;
    } ParamDecl;

    struct
    {
        ASTNode *callee, **args;
        int param_count;
    } CallExpr;

    struct
    {
        ASTNode **elements;
        int elem_count;
    } ArrayLiteral;

    struct
    {
        ASTNode *inner_type;
    } ArrayType;

    struct
    {
        ASTNode *object;
        const char *property;
    } PropertyAccess;

    struct
    {
        ASTNode *cond, *arms;
    } MatchExpr;

    struct
    {
        ASTNode *pattern, *body, *next;
    } MatchArm;
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
ASTNode *create_param_node(const char *name, ASTNode *type);
ASTNode *create_function_node(const char *name, ASTNode **params, int param_count, ASTNode *returnType, ASTNode *body);
ASTNode *create_call_node(ASTNode *callee, ASTNode **args, int count);
ASTNode *create_array_node(ASTNode **elems, int count);
ASTNode *create_array_type_node(ASTNode *inner);
ASTNode *create_property_access_node(ASTNode *object, const char *property);
ASTNode *create_match_arms_node(ASTNode *pat, ASTNode *body, ASTNode *next);
ASTNode *create_match_node(ASTNode *cond, ASTNode *arms);
ASTNode *append_match_arm(ASTNode *arms, ASTNode *pat, ASTNode *body);
ASTNode *wrap_block_if_needed(ASTNode *list);
ASTNode *create_block_node_append(ASTNode *expr);

void indent_print(int indentation, const char *fmt, ...);
void printAST(ASTNode *node, int indentation);
void freeAST(ASTNode *node);
