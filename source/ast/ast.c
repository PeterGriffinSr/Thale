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

ASTNode *create_param_node(const char *name, ASTNode *type)
{
    ASTNode *node = new_node(NodeParam);
    node->ParamDecl.name = name;
    node->ParamDecl.typeExpr = type;
    return node;
}

ASTNode *create_function_node(const char *name, ASTNode **params, int param_count, ASTNode *returnType, ASTNode *body)
{
    ASTNode *node = new_node(NodeFunctionDecl);
    node->FunctionDecl.name = name;
    node->FunctionDecl.params = params;
    node->FunctionDecl.param_count = param_count;
    node->FunctionDecl.returnType = returnType;
    node->FunctionDecl.body = body;
    node->FunctionDecl.is_recursive = 0;
    return node;
}

ASTNode *create_call_node(ASTNode *callee, ASTNode **args, int count)
{
    ASTNode *node = new_node(NodeCallExpr);
    node->CallExpr.callee = callee;
    node->CallExpr.args = args;
    node->CallExpr.param_count = count;
    return node;
}

ASTNode *create_array_node(ASTNode **elems, int count)
{
    ASTNode *node = new_node(NodeArrayLiteral);
    node->ArrayLiteral.elements = elems;
    node->ArrayLiteral.elem_count = count;
    return node;
}

ASTNode *create_array_type_node(ASTNode *inner)
{
    ASTNode *node = new_node(NodeArrayType);
    node->ArrayType.inner_type = inner;
    return node;
}

ASTNode *create_property_access_node(ASTNode *object, const char *property)
{
    ASTNode *node = new_node(NodePropertyAccess);
    node->PropertyAccess.object = object;
    node->PropertyAccess.property = property;
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
    case NodeParam:
        indent_print(indentation, "Param: %s\n", node->ParamDecl.name);
        indent_print(indentation, "Type:\n");
        printAST(node->ParamDecl.typeExpr, indentation + 2);
        break;
    case NodeFunctionDecl:
        if (node->FunctionDecl.is_recursive)
        {
            indent_print(indentation, "FunctionDecl (rec): %s\n", node->FunctionDecl.name);
        }
        else
        {
            indent_print(indentation, "FunctionDecl: %s\n", node->FunctionDecl.name);
        }
        indent_print(indentation, "Params:\n");
        for (int i = 0; i < node->FunctionDecl.param_count; ++i)
            printAST(node->FunctionDecl.params[i], indentation + 2);

        indent_print(indentation, "ReturnType:\n");
        printAST(node->FunctionDecl.returnType, indentation + 2);

        indent_print(indentation, "Body:\n");
        printAST(node->FunctionDecl.body, indentation + 2);
        break;
    case NodeCallExpr:
        indent_print(indentation, "CallExpr:\n");
        printAST(node->CallExpr.callee, indentation + 2);
        for (int i = 0; i < node->CallExpr.param_count; i++)
        {
            indent_print(indentation + 2, "Arg %d:\n", i);
            printAST(node->CallExpr.args[i], indentation + 4);
        }
        break;
    case NodeArrayLiteral:
        indent_print(indentation, "ArrayLiteral:\n");
        for (int i = 0; i < node->ArrayLiteral.elem_count; ++i)
            printAST(node->ArrayLiteral.elements[i], indentation + 2);
        break;
    case NodeArrayType:
        indent_print(indentation, "ArrayType:\n");
        printAST(node->ArrayType.inner_type, indentation + 2);
        break;
    case NodePropertyAccess:
        indent_print(indentation, "PropertyAccess:\n");
        printAST(node->PropertyAccess.object, indentation + 2);
        indent_print(indentation + 2, "Property: %s\n", node->PropertyAccess.property);
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
    case NodeParam:
        free((char *)node->ParamDecl.name);
        freeAST(node->ParamDecl.typeExpr);
        break;
    case NodeFunctionDecl:
        for (int i = 0; i < node->FunctionDecl.param_count; ++i)
            freeAST(node->FunctionDecl.params[i]);
        free(node->FunctionDecl.params);
        freeAST(node->FunctionDecl.returnType);
        freeAST(node->FunctionDecl.body);
        free((char *)node->FunctionDecl.name);
        break;
    case NodeCallExpr:
        freeAST(node->CallExpr.callee);
        for (int i = 0; i < node->CallExpr.param_count; i++)
        {
            freeAST(node->CallExpr.args[i]);
        }
        free(node->CallExpr.args);
        break;
    case NodeArrayLiteral:
        for (int i = 0; i < node->ArrayLiteral.elem_count; ++i)
            freeAST(node->ArrayLiteral.elements[i]);
        free(node->ArrayLiteral.elements);
        break;
    case NodeArrayType:
        freeAST(node->ArrayType.inner_type);
        break;
    case NodePropertyAccess:
        freeAST(node->PropertyAccess.object);
        free((char *)node->PropertyAccess.property);
        break;
    default:
        break;
    }

    free(node);
}
