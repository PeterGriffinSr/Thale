#include <stdio.h>
#include <stdlib.h>

#include "ast.h"
#include "error.h"
#include "parser.yy.h"

extern ASTNode *root;
#if defined(_WIN32) || defined(__APPLE__)
extern int yyparse(void);
#endif
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
    yyfilename = filename;

    if (yyparse() == 0)
    {
        printAST(root, 0);
        freeAST(root);
    }
    yylex_destroy();
    fclose(file);

    return EXIT_SUCCESS;
}