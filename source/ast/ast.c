#include "ast.h"
#include <stdio.h>
#include <stdarg.h>
#include <stdlib.h>
#include <string.h>

static ASTNode *new_node(NodeType type)
{
    ASTNode *node = (ASTNode *)malloc(sizeof(ASTNode));
    if (!node)
    {
        fprintf(stderr, "Out of memory!\n");
        exit(1);
    }
    node->type = type;
    return node;
}

#define DEFINE_LITERAL_CREATOR(func, nodetype, field, ctype) \
    ASTNode *func(ctype value)                               \
    {                                                        \
        ASTNode *node = new_node(nodetype);                  \
        node->field = value;                                 \
        return node;                                         \
    }

DEFINE_LITERAL_CREATOR(create_int_node, NodeIntLiteral, IntVal, int)
DEFINE_LITERAL_CREATOR(create_float_node, NodeFloatLiteral, FloatVal, double)
DEFINE_LITERAL_CREATOR(create_char_node, NodeCharLiteral, CharVal, char)
DEFINE_LITERAL_CREATOR(create_bool_node, NodeBoolLiteral, BoolVal, bool)

ASTNode *create_string_node(const char *value)
{
    ASTNode *node = new_node(NodeStringLiteral);
    node->StrVal = strdup(value);
    return node;
}

ASTNode *create_identifier_node(const char *value)
{
    ASTNode *node = new_node(NodeIdentifier);
    node->StrVal = (char *)value;
    return node;
}

ASTNode *create_block_node(void)
{
    ASTNode *node = new_node(NodeBlock);
    node->BlockExpr.count = 0;
    node->BlockExpr.cap = 4;
    node->BlockExpr.exprs =
        (ASTNode **)malloc(sizeof(ASTNode *) * (size_t)node->BlockExpr.cap);
    return node;
}

ASTNode *append_expr(ASTNode *block, ASTNode *expr)
{
    if (!block)
        block = create_block_node();
    if (block->type != NodeBlock)
    {
        ASTNode *new_block = create_block_node();
        new_block->BlockExpr.exprs[new_block->BlockExpr.count++] = block;
        block = new_block;
    }

    if (block->BlockExpr.count >= block->BlockExpr.cap)
    {
        block->BlockExpr.cap *= 2;
        block->BlockExpr.exprs =
            (ASTNode **)realloc(block->BlockExpr.exprs,
                                sizeof(ASTNode *) * (size_t)block->BlockExpr.cap);
    }

    block->BlockExpr.exprs[block->BlockExpr.count++] = expr;
    return block;
}

ASTNode *create_binary_node(const char *op, ASTNode *left, ASTNode *right)
{
    ASTNode *node = new_node(NodeBinary);
    node->BinaryExpr.op = op;
    node->BinaryExpr.left = left;
    node->BinaryExpr.right = right;
    return node;
}

ASTNode *create_unary_node(const char *op, ASTNode *left)
{
    ASTNode *node = new_node(NodeUnary);
    node->UnaryExpr.op = op;
    node->UnaryExpr.left = left;
    return node;
}

ASTNode *create_var_decl_node(const char *name, ASTNode *typeExpr, ASTNode *value)
{
    ASTNode *node = new_node(NodeVarDecl);
    node->VarDecl.name = name;
    node->VarDecl.typeExpr = typeExpr;
    node->VarDecl.value = value;
    return node;
}

ASTNode *create_type_node(const char *name)
{
    ASTNode *node = new_node(NodeTypeExpr);
    node->StrVal = strdup(name);
    return node;
}

void indent_print(int indentation, const char *fmt, ...)
{
    va_list args;
    for (int i = 0; i < indentation; i++)
        printf(" ");

    va_start(args, fmt);
    vprintf(fmt, args);
    va_end(args);
}

void printAST(ASTNode *node, int indentation)
{
    if (!node)
    {
        indent_print(indentation, "NULL\n");
        return;
    }

    switch (node->type)
    {
    case NodeIntLiteral:
        indent_print(indentation, "IntLiteral: %d\n", node->IntVal);
        break;
    case NodeFloatLiteral:
        indent_print(indentation, "FloatLiteral: %lf\n", node->FloatVal);
        break;
    case NodeCharLiteral:
        indent_print(indentation, "CharLiteral: %c\n", node->CharVal);
        break;
    case NodeStringLiteral:
        indent_print(indentation, "StringLiteral: %s\n", node->StrVal);
        break;
    case NodeBoolLiteral:
        indent_print(indentation, "BoolLiteral: %d\n", node->BoolVal);
        break;
    case NodeIdentifier:
        indent_print(indentation, "Identifier: %s\n", node->StrVal);
        break;
    case NodeBlock:
        indent_print(indentation, "Block:\n");
        for (int i = 0; i < node->BlockExpr.count; ++i)
            printAST(node->BlockExpr.exprs[i], indentation + 1);
        break;
    case NodeBinary:
        indent_print(indentation, "BinaryOp: '%s'\n", node->BinaryExpr.op);
        printAST(node->BinaryExpr.left, indentation + 1);
        printAST(node->BinaryExpr.right, indentation + 1);
        break;
    case NodeUnary:
        indent_print(indentation, "UnaryOp: '%s'\n", node->UnaryExpr.op);
        printAST(node->UnaryExpr.left, indentation + 1);
        break;
    case NodeVarDecl:
        indent_print(indentation, "VarDecl: %s\n", node->VarDecl.name);
        indent_print(indentation, "Type:\n");
        printAST(node->VarDecl.typeExpr, indentation + 2);
        indent_print(indentation, "Value:\n");
        printAST(node->VarDecl.value, indentation + 2);
        break;
    case NodeTypeExpr:
        indent_print(indentation, "TypeExpr: %s\n", node->StrVal);
        break;
    default:
        indent_print(indentation, "Unknown node\n");
        break;
    }
}

void freeAST(ASTNode *node)
{
    if (!node)
        return;

    switch (node->type)
    {
    case NodeIntLiteral:
    case NodeBoolLiteral:
    case NodeFloatLiteral:
    case NodeCharLiteral:
        break;
    case NodeStringLiteral:
        free((char *)node->StrVal);
        break;
    case NodeIdentifier:
        free((char *)node->StrVal);
        break;
    case NodeBlock:
        for (int i = 0; i < node->BlockExpr.count; ++i)
            freeAST(node->BlockExpr.exprs[i]);
        free(node->BlockExpr.exprs);
        break;
    case NodeBinary:
        freeAST(node->BinaryExpr.left);
        freeAST(node->BinaryExpr.right);
        break;
    case NodeUnary:
        freeAST(node->UnaryExpr.left);
        break;
    case NodeVarDecl:
        freeAST(node->VarDecl.typeExpr);
        freeAST(node->VarDecl.value);
        free((char *)node->VarDecl.name);
        break;
    case NodeTypeExpr:
        free((char *)node->StrVal);
        break;
    default:
        break;
    }

    free(node);
}
