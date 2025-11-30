#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include <string.h>

#include "tokens.h"
#include "symbol_table.h"

/* ---- externs coming from lex.yy.c (scanner) ---- */
extern int   yylex(void);
extern char *yytext;
extern FILE *yyin;

/* Line counter is defined in scanner.l */
extern int line;

/* Current lookahead token */
static int lookahead;

/* ======== Helper structs for expression values ======== */

typedef struct {
    VarType type;
    float   fval;   /* we store both int and float numerically as float */
} ExprVal;

/* ======== Error handling & parsing utilities ======== */

static void syntax_error(const char *expected) {
    fprintf(stderr,
            "Syntax error at line %d: expected %s, found %s ('%s')\n",
            line, expected, token_name(lookahead), yytext);
    exit(EXIT_FAILURE);
}

static void advance(void) {
    lookahead = yylex();
}

static void match(int expected) {
    if (lookahead == expected) {
        advance();
    } else {
        char buf[64];
        snprintf(buf, sizeof(buf), "'%s'", token_name(expected));
        syntax_error(buf);
    }
}

/* Forward declarations for nonterminals */
static void program_(void);
static void declaration_list(void);
static void declaration(void);
static VarType type_specifier(void);
static void statement_list(int exec);
static void statement(int exec);
static void compound_stmt(int exec);
static void assignment_stmt(int exec);
static void selection_stmt(int exec);
static void iteration_stmt(int exec);
static void expression(ExprVal *out);
static void additive_expression(ExprVal *out);
static void term(ExprVal *out);
static void factor(ExprVal *out);

/* Utility: require that two ExprVal have the same type */
static void ensure_same_type(const ExprVal *a, const ExprVal *b) {
    if (a->type != b->type) {
        semantic_error("Type mismatch: operands are not the same type");
    }
}

/* Convert ExprVal to boolean (0 or 1) for conditions */
static int expr_to_bool(const ExprVal *e) {
    /* We treat zero as false, non-zero as true, regardless of type */
    return (e->fval != 0.0f);
}

/* ======== Grammar implementation ======== */

/* program → program ID { declaration-list statement-list } */
static void program_(void) {
    if (lookahead != T_PROGRAM) {
        syntax_error("'program'");
    }
    match(T_PROGRAM);

    if (lookahead != T_ID) {
        syntax_error("program identifier");
    }
    /* We don't need to store the program name; just consume it. */
    match(T_ID);

    match(T_LBRACE);
    declaration_list();
    statement_list(/*exec=*/1);
    match(T_RBRACE);
}

/* declaration-list → { declaration } */
static void declaration_list(void) {
    while (lookahead == T_INT || lookahead == T_FLOAT) {
        declaration();
    }
}

/* declaration → type-specifier ID ; */
static void declaration(void) {
    VarType t = type_specifier();

    if (lookahead != T_ID) {
        syntax_error("identifier");
    }
    char name[64];
    /* yytext contains the current lexeme for lookahead */
    strncpy(name, yytext, sizeof(name) - 1);
    name[sizeof(name) - 1] = '\0';

    match(T_ID);
    match(T_SEMI);

    /* Insert into symbol table */
    st_insert(name, t, line);
}

/* type-specifier → int | float */
static VarType type_specifier(void) {
    if (lookahead == T_INT) {
        match(T_INT);
        return TYPE_INT;
    } else if (lookahead == T_FLOAT) {
        match(T_FLOAT);
        return TYPE_FLOAT;
    } else {
        syntax_error("type specifier (int or float)");
        return TYPE_INT; /* unreachable */
    }
}

/* statement-list → { statement } */
static void statement_list(int exec) {
    while (lookahead == T_ID     ||
           lookahead == T_IF     ||
           lookahead == T_WHILE  ||
           lookahead == T_LBRACE ||
           lookahead == T_SEMI) {
        statement(exec);
    }
}

/* statement → assignment-stmt | selection-stmt | iteration-stmt
 *           | compound-stmt | ;   (empty statement)
 */
static void statement(int exec) {
    switch (lookahead) {
        case T_ID:
            assignment_stmt(exec);
            break;
        case T_IF:
            selection_stmt(exec);
            break;
        case T_WHILE:
            iteration_stmt(exec);
            break;
        case T_LBRACE:
            compound_stmt(exec);
            break;
        case T_SEMI:
            /* Empty statement */
            match(T_SEMI);
            break;
        default:
            syntax_error("statement");
    }
}

/* compound-stmt → { statement-list } */
static void compound_stmt(int exec) {
    match(T_LBRACE);
    statement_list(exec);
    match(T_RBRACE);
}

/* assignment-stmt → ID = expression ; */
static void assignment_stmt(int exec) {
    if (lookahead != T_ID) {
        syntax_error("identifier at start of assignment");
    }

    char name[64];
    strncpy(name, yytext, sizeof(name) - 1);
    name[sizeof(name) - 1] = '\0';

    match(T_ID);
    match(T_ASSIGN);

    ExprVal rhs;
    expression(&rhs);

    match(T_SEMI);

    if (!exec) {
        /* We are in a "skipped" branch – just parse, no side effects */
        return;
    }

    /* Look up variable and assign */
    Symbol *sym = st_lookup(name);
    if (sym->type != rhs.type) {
        semantic_error("Type mismatch in assignment to '%s'", name);
    }

    if (rhs.type == TYPE_INT) {
        st_set_int(sym, (int)rhs.fval);
    } else {
        st_set_float(sym, rhs.fval);
    }
}

/* selection-stmt → if ( expression ) statement [ else statement ] */
static void selection_stmt(int exec) {
    match(T_IF);
    match(T_LPAREN);

    ExprVal cond;
    expression(&cond);

    match(T_RPAREN);

    int cond_true = expr_to_bool(&cond);

    if (!exec) {
        /* Already in a skipped context: parse both branches with exec=0 */
        statement(0);
        if (lookahead == T_ELSE) {
            match(T_ELSE);
            statement(0);
        }
        return;
    }

    if (cond_true) {
        /* Execute then-branch, skip else-branch if present */
        statement(1);
        if (lookahead == T_ELSE) {
            match(T_ELSE);
            statement(0);   /* parse but do not execute else */
        }
    } else {
        /* Skip then-branch, execute else-branch (if present) */
        statement(0);
        if (lookahead == T_ELSE) {
            match(T_ELSE);
            statement(1);
        }
    }
}

/* iteration-stmt → while ( expression ) statement
 *
 * NOTE: Because this is a single-pass interpreter that executes during parsing,
 * we cannot "loop back" over the input without building an explicit tree or
 * re-reading the source. For simplicity, we execute the loop body at most once.
 * You can extend this to full loop semantics later if needed.
 */
static void iteration_stmt(int exec) {
    match(T_WHILE);
    match(T_LPAREN);

    ExprVal cond;
    expression(&cond);

    match(T_RPAREN);

    int cond_true = expr_to_bool(&cond);

    if (!exec || !cond_true) {
        /* Not executing at all, or condition false: parse body, no side effects */
        statement(0);
    } else {
        /* Condition true and exec=1: execute body once */
        statement(1);
    }
}

/* expression → additive-expression [ relop additive-expression ]
 *
 * The result of a relational expression is of type int (0 or 1).
 */
static void expression(ExprVal *out) {
    ExprVal left;
    additive_expression(&left);

    /* Check for relational operator */
    if (lookahead == T_EQ  || lookahead == T_NEQ ||
        lookahead == T_LT  || lookahead == T_LTE ||
        lookahead == T_GT  || lookahead == T_GTE) {

        int op = lookahead;
        match(lookahead);

        ExprVal right;
        additive_expression(&right);

        ensure_same_type(&left, &right);

        out->type = TYPE_INT;

        switch (op) {
            case T_EQ:  out->fval = (left.fval == right.fval); break;
            case T_NEQ: out->fval = (left.fval != right.fval); break;
            case T_LT:  out->fval = (left.fval <  right.fval); break;
            case T_LTE: out->fval = (left.fval <= right.fval); break;
            case T_GT:  out->fval = (left.fval >  right.fval); break;
            case T_GTE: out->fval = (left.fval >= right.fval); break;
        }
    } else {
        /* Just an additive expression */
        *out = left;
    }
}

/* additive-expression → term { (+|-) term } */
static void additive_expression(ExprVal *out) {
    ExprVal left;
    term(&left);

    while (lookahead == T_PLUS || lookahead == T_MINUS) {
        int op = lookahead;
        match(lookahead);

        ExprVal right;
        term(&right);

        ensure_same_type(&left, &right);

        if (left.type == TYPE_INT) {
            int li = (int)left.fval;
            int ri = (int)right.fval;
            left.fval = (op == T_PLUS) ? (li + ri) : (li - ri);
        } else { /* TYPE_FLOAT */
            left.fval = (op == T_PLUS)
                        ? (left.fval + right.fval)
                        : (left.fval - right.fval);
        }
    }

    *out = left;
}

/* term → factor { (*|/) factor } */
static void term(ExprVal *out) {
    ExprVal left;
    factor(&left);

    while (lookahead == T_MULT || lookahead == T_DIV) {
        int op = lookahead;
        match(lookahead);

        ExprVal right;
        factor(&right);

        ensure_same_type(&left, &right);

        if (left.type == TYPE_INT) {
            int li = (int)left.fval;
            int ri = (int)right.fval;
            if (op == T_MULT) {
                left.fval = li * ri;
            } else {
                if (ri == 0) {
                    semantic_error("Division by zero");
                }
                left.fval = li / ri; /* integer division */
            }
        } else { /* TYPE_FLOAT */
            if (op == T_MULT) {
                left.fval = left.fval * right.fval;
            } else {
                if (right.fval == 0.0f) {
                    semantic_error("Division by zero");
                }
                left.fval = left.fval / right.fval;
            }
        }
    }

    *out = left;
}

/* factor → ( expression ) | ID | NUM */
static void factor(ExprVal *out) {
    if (lookahead == T_LPAREN) {
        match(T_LPAREN);
        expression(out);
        match(T_RPAREN);
    } else if (lookahead == T_ID) {
        char name[64];
        strncpy(name, yytext, sizeof(name) - 1);
        name[sizeof(name) - 1] = '\0';
        match(T_ID);

        Symbol *sym = st_lookup(name);

        out->type = sym->type;
        if (sym->type == TYPE_INT) {
            out->fval = (float)st_get_int(sym);
        } else {
            out->fval = st_get_float(sym);
        }
    } else if (lookahead == T_NUM) {
        /* Convert lexeme to numeric value BEFORE matching, because match() advances */
        char buf[64];
        strncpy(buf, yytext, sizeof(buf) - 1);
        buf[sizeof(buf) - 1] = '\0';
        match(T_NUM);

        /* Decide type: if it contains '.', treat as float; otherwise int */
        int is_float = (strchr(buf, '.') != NULL);

        if (is_float) {
            out->type = TYPE_FLOAT;
            out->fval = (float)atof(buf);
        } else {
            out->type = TYPE_INT;
            out->fval = (float)atoi(buf);
        }
    } else {
        syntax_error("factor");
    }
}

/* ======== main driver ======== */

int main(int argc, char **argv) {
    if (argc > 1) {
        yyin = fopen(argv[1], "r");
        if (!yyin) {
            fprintf(stderr, "Cannot open input file '%s'\n", argv[1]);
            return EXIT_FAILURE;
        }
    } else {
        yyin = stdin;
    }

    st_init();

    advance();      /* initialize lookahead */
    program_();     /* start symbol */

    if (lookahead != T_EOF) {
        syntax_error("EOF");
    }

    printf("Interpretation completed successfully.\n");
    st_print_all();

    if (yyin && yyin != stdin) {
        fclose(yyin);
    }
    return EXIT_SUCCESS;
}
