#include <stdio.h>
#include <stdlib.h>
#include "parser.yy.h"
#include "ast.h"

extern ASTNode *root;
extern int yylex_destroy();
extern FILE *yyin;

int main(int argc, char **argv)
{
    if (argc < 2)
    {
        fprintf(stderr, "Error: requires a file argument.\n");
        return EXIT_FAILURE;
    }
    const char *filename = argv[1];
    FILE *file = fopen(filename, "r");
    yyin = file;
    root = NULL;
    if (yyparse() == 0)
    {
        printAST(root, 0);
        freeAST(root);
    }
    yylex_destroy();
    fclose(file);

    return EXIT_SUCCESS;
}