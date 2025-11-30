#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdarg.h>

#include "symbol_table.h"

/* line is defined by the scanner and used for error messages */
extern int line;

#define MAX_SYMBOLS 1024

static Symbol symtab[MAX_SYMBOLS];
static int    sym_count = 0;

void st_init(void) {
    sym_count = 0;
}

/* Find symbol index by name; -1 if not found */
static int st_find_index(const char *name) {
    for (int i = 0; i < sym_count; ++i) {
        if (strcmp(symtab[i].name, name) == 0) {
            return i;
        }
    }
    return -1;
}

Symbol *st_insert(const char *name, VarType type, int decl_line) {
    if (sym_count >= MAX_SYMBOLS) {
        fprintf(stderr, "Symbol table overflow\n");
        exit(EXIT_FAILURE);
    }

    int idx = st_find_index(name);
    if (idx != -1) {
        /* You can choose to allow redeclaration; here we treat as error */
        semantic_error("Variable '%s' redeclared (first declared at line %d)",
                       name, symtab[idx].decl_line);
    }

    Symbol *s = &symtab[sym_count++];
    strncpy(s->name, name, sizeof(s->name) - 1);
    s->name[sizeof(s->name) - 1] = '\0';
    s->type        = type;
    s->decl_line   = decl_line;
    s->initialized = 0;
    s->value.i_val = 0;
    s->value.f_val = 0.0f;
    return s;
}

Symbol *st_lookup(const char *name) {
    int idx = st_find_index(name);
    if (idx == -1) {
        semantic_error("Use of undeclared variable '%s'", name);
    }
    return &symtab[idx];
}

void st_set_int(Symbol *sym, int v) {
    if (!sym) return;
    if (sym->type != TYPE_INT) {
        semantic_error("Assigning int value to non-int variable '%s'", sym->name);
    }
    sym->value.i_val = v;
    sym->initialized = 1;
}

void st_set_float(Symbol *sym, float v) {
    if (!sym) return;
    if (sym->type != TYPE_FLOAT) {
        semantic_error("Assigning float value to non-float variable '%s'", sym->name);
    }
    sym->value.f_val = v;
    sym->initialized = 1;
}

int st_get_int(Symbol *sym) {
    if (!sym) {
        semantic_error("Internal error: NULL symbol in st_get_int");
    }
    if (sym->type != TYPE_INT) {
        semantic_error("Using non-int variable '%s' as int", sym->name);
    }
    if (!sym->initialized) {
        semantic_error("Using uninitialized int variable '%s'", sym->name);
    }
    return sym->value.i_val;
}

float st_get_float(Symbol *sym) {
    if (!sym) {
        semantic_error("Internal error: NULL symbol in st_get_float");
    }
    if (sym->type != TYPE_FLOAT) {
        semantic_error("Using non-float variable '%s' as float", sym->name);
    }
    if (!sym->initialized) {
        semantic_error("Using uninitialized float variable '%s'", sym->name);
    }
    return sym->value.f_val;
}

void st_print_all(void) {
    printf("\n----- Symbol Table -----\n");
    printf("%-16s %-8s %-12s %-8s\n", "Name", "Type", "Value", "DeclLn");
    printf("---------------------------------------------\n");
    for (int i = 0; i < sym_count; ++i) {
        const Symbol *s = &symtab[i];
        const char *tname = (s->type == TYPE_INT) ? "int" : "float";
        printf("%-16s %-8s ", s->name, tname);
        if (!s->initialized) {
            printf("%-12s ", "(uninit)");
        } else if (s->type == TYPE_INT) {
            printf("%-12d ", s->value.i_val);
        } else {
            printf("%-12.3f ", s->value.f_val);
        }
        printf("%-8d\n", s->decl_line);
    }
    printf("---------------------------------------------\n");
}

/* Print semantic error with line number and exit */
void semantic_error(const char *fmt, ...) {
    va_list ap;
    fprintf(stderr, "Semantic error at line %d: ", line);
    va_start(ap, fmt);
    vfprintf(stderr, fmt, ap);
    va_end(ap);
    fprintf(stderr, "\n");
    exit(EXIT_FAILURE);
}
