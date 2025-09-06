#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdarg.h>
#include "error.h"

#define RED "\x1b[31m"
#define YELLOW "\x1b[33m"
#define BOLD "\x1b[1m"
#define RESET "\x1b[0m"

int yycolumn = 1;
const char *yyfilename = NULL;
int context_lines = 2;

static void print_error_context(const char *file, int line, int column, int length)
{
    FILE *fp = fopen(file, "r");
    if (!fp)
        return;

    int start = (line - context_lines > 1) ? line - context_lines : 1;
    int end = line + context_lines;
    char *buf = NULL;
    size_t len = 0;
    int current = 1;

    while (getline(&buf, &len, fp) != -1)
    {
        if (current >= start && current <= end)
        {
            size_t n = strlen(buf);
            if (n > 0 && buf[n - 1] == '\n')
                buf[n - 1] = '\0';
            fprintf(stderr, "%4d | %s\n", current, buf);
            if (current == line)
            {
                fprintf(stderr, "      %*s", column, "");
                for (int i = 0; i < length && i + column <= (int)strlen(buf); i++)
                    fprintf(stderr, BOLD RED "^");
                fprintf(stderr, RESET "\n");
            }
        }
        current++;
    }

    free(buf);
    fclose(fp);
}

static void vreport_error(const char *prefix, const ErrorLocation *loc, const char *fmt, va_list args)
{
    fprintf(stderr, "%s:%d:%d: %s%s error:%s ",
            loc->file ? loc->file : "<stdin>",
            loc->line, loc->column,
            RED, prefix, RESET);
    vfprintf(stderr, fmt, args);
    fprintf(stderr, "\n");

    if (loc->file)
        print_error_context(loc->file, loc->line, loc->column, 1);
}

void lexer_error(const ErrorLocation *loc, const char *fmt, ...)
{
    va_list args;
    va_start(args, fmt);
    vreport_error("Lexer", loc, fmt, args);
    va_end(args);
    exit(EXIT_FAILURE);
}

ErrorLocation make_location(const char *file, int line, int column)
{
    ErrorLocation loc;
    loc.file = file;
    loc.line = line;
    loc.column = column;
    return loc;
}
