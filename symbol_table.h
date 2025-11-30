#ifndef SYMBOL_TABLE_H
#define SYMBOL_TABLE_H

#include "tokens.h"

/* Variable type */
typedef enum {
    TYPE_INT = 1,
    TYPE_FLOAT = 2
} VarType;

/* One symbol table entry */
typedef struct {
    char    name[64];
    VarType type;
    int     decl_line;
    int     initialized;
    union {
        int   i_val;
        float f_val;
    } value;
} Symbol;

/* Simple flat symbol table interface */
void    st_init(void);
Symbol* st_insert(const char *name, VarType type, int decl_line);
Symbol* st_lookup(const char *name);

/* Set / get helpers */
void    st_set_int(Symbol *sym, int v);
void    st_set_float(Symbol *sym, float v);
int     st_get_int(Symbol *sym);
float   st_get_float(Symbol *sym);

/* Debug: print all variables and their values */
void    st_print_all(void);

/* Semantic error helper – prints line number and exits */
void    semantic_error(const char *fmt, ...);

#endif /* SYMBOL_TABLE_H */
