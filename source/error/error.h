#pragma once

#include <stdarg.h>
#include <stdbool.h>

extern int yycolumn;
extern const char *yyfilename;

typedef struct
{
    int line, column;
    const char *file;
} ErrorLocation;

void lexer_error(const ErrorLocation *loc, const char *fmt, ...);
ErrorLocation make_location(const char *file, int line, int column);