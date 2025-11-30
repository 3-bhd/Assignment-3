#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include <string.h>

#include "tokens.h"
#include "symbol_table.h"

/* External scanner definitions */
extern int   yylex(void);
extern char *yytext;
extern FILE *yyin;
extern int line;

/* Current lookahead token */
static int lookahead;

/* Simple safe strdup replacement (portable) */
static char *my_strdup(const char *s) {
    size_t n = strlen(s) + 1;
    char *p = (char *)malloc(n);
    if (!p) {
        fprintf(stderr, "Out of memory in my_strdup\n");
        exit(EXIT_FAILURE);
    }
    memcpy(p, s, n);
    return p;
}

/* ---------------------------------
   Runtime value for evaluating Expr
---------------------------------- */
typedef struct {
    VarType type;
    float   fval;  /* store int and float as float numerically */
} ExprVal;

/* Forward declarations of AST node structs */
typedef struct Expr Expr;
typedef struct Stmt Stmt;

/* =======================
   AST node declarations
   ======================= */

typedef enum {
    EXPR_VAR,
    EXPR_NUM,
    EXPR_BINOP
} ExprKind;

typedef enum {
    OP_ADD, OP_SUB, OP_MUL, OP_DIV,
    OP_EQ, OP_NEQ, OP_LT, OP_LTE, OP_GT, OP_GTE
} BinOpKind;

struct Expr {
    ExprKind kind;
    VarType  type;   /* static type of the expression */
    union {
        struct { char *name; } var;
        struct { float value; } num;
        struct {
            BinOpKind op;
            Expr *left;
            Expr *right;
        } bin;
    } u;
};

typedef enum {
    STMT_ASSIGN,
    STMT_IF,
    STMT_WHILE,
    STMT_BLOCK,
    STMT_EMPTY
} StmtKind;

struct Stmt {
    StmtKind kind;
    Stmt *next;         /* linked list for statement lists */

    union {
        struct {
            char *name;
            Expr *expr;
        } assign;

        struct {
            Expr *cond;
            Stmt *then_branch;
            Stmt *else_branch;
        } sel;

        struct {
            Expr *cond;
            Stmt *body;
        } wh;

        struct {
            Stmt *body;
        } block;

        /* STMT_EMPTY has no payload */
    } u;
};

/* =======================
   Error & parse utilities
   ======================= */

static void syntax_error(const char *expected) {
    fprintf(stderr,
        "Syntax error at line %d: expected %s but found '%s'\n",
        line, expected, yytext);
    exit(EXIT_FAILURE);
}

static void advance(void) {
    lookahead = yylex();
}

static void match(int expected) {
    if (lookahead == expected) {
        advance();
    } else {
        syntax_error(token_name(expected));
    }
}

static void ensure_same_type(const ExprVal *a, const ExprVal *b) {
    if (a->type != b->type) {
        semantic_error("Type mismatch in expression");
    }
}

static int expr_to_bool(const ExprVal *e) {
    return (e->fval != 0.0f);
}

/* =======================
   Forward declarations
   ======================= */

/* Parsing (build AST) */
static void    program_(void);
static void    declaration_list(void);
static void    declaration(void);
static VarType type_specifier(void);

static Stmt  *statement_list(void);
static Stmt  *statement(void);
static Stmt  *compound_stmt(void);
static Stmt  *assignment_stmt(void);
static Stmt  *selection_stmt(void);
static Stmt  *iteration_stmt(void);

static Expr  *expression(void);
static Expr  *additive_expression(void);
static Expr  *term(void);
static Expr  *factor(void);

/* Execution */
static void   exec_stmt_list(Stmt *s);
static void   exec_stmt(Stmt *s);
static void   eval_expr(const Expr *e, ExprVal *out);

/* AST constructors */
static Expr *make_var_expr(const char *name) {
    Expr *e = (Expr *)malloc(sizeof(Expr));
    if (!e) {
        fprintf(stderr, "Out of memory in make_var_expr\n");
        exit(EXIT_FAILURE);
    }
    e->kind = EXPR_VAR;

    /* look up now to set the type + catch undeclared vars */
    Symbol *sym = st_lookup(name);
    e->type = sym->type;

    e->u.var.name = my_strdup(name);
    return e;
}

static Expr *make_num_expr(VarType t, float value) {
    Expr *e = (Expr *)malloc(sizeof(Expr));
    if (!e) {
        fprintf(stderr, "Out of memory in make_num_expr\n");
        exit(EXIT_FAILURE);
    }
    e->kind = EXPR_NUM;
    e->type = t;
    e->u.num.value = value;
    return e;
}

static Expr *make_binop_expr(BinOpKind op, VarType type, Expr *l, Expr *r) {
    Expr *e = (Expr *)malloc(sizeof(Expr));
    if (!e) {
        fprintf(stderr, "Out of memory in make_binop_expr\n");
        exit(EXIT_FAILURE);
    }
    e->kind = EXPR_BINOP;
    e->type = type;
    e->u.bin.op   = op;
    e->u.bin.left = l;
    e->u.bin.right= r;
    return e;
}

static Stmt *alloc_stmt(StmtKind k) {
    Stmt *s = (Stmt *)malloc(sizeof(Stmt));
    if (!s) {
        fprintf(stderr, "Out of memory in alloc_stmt\n");
        exit(EXIT_FAILURE);
    }
    s->kind = k;
    s->next = NULL;
    return s;
}

static Stmt *make_empty_stmt(void) {
    return alloc_stmt(STMT_EMPTY);
}

static Stmt *make_assign_stmt(const char *name, Expr *expr) {
    Stmt *s = alloc_stmt(STMT_ASSIGN);
    s->u.assign.name = my_strdup(name);
    s->u.assign.expr = expr;
    return s;
}

static Stmt *make_if_stmt(Expr *cond, Stmt *t, Stmt *e) {
    Stmt *s = alloc_stmt(STMT_IF);
    s->u.sel.cond        = cond;
    s->u.sel.then_branch = t;
    s->u.sel.else_branch = e;
    return s;
}

static Stmt *make_while_stmt(Expr *cond, Stmt *body) {
    Stmt *s = alloc_stmt(STMT_WHILE);
    s->u.wh.cond = cond;
    s->u.wh.body = body;
    return s;
}

static Stmt *make_block_stmt(Stmt *body) {
    Stmt *s = alloc_stmt(STMT_BLOCK);
    s->u.block.body = body;
    return s;
}

/* =======================
   Expression evaluation
   ======================= */

static void eval_expr(const Expr *e, ExprVal *out) {
    switch (e->kind) {

    case EXPR_NUM:
        out->type = e->type;
        out->fval = e->u.num.value;
        break;

    case EXPR_VAR: {
        Symbol *sym = st_lookup(e->u.var.name);
        out->type = sym->type;
        if (sym->type == TYPE_INT)
            out->fval = st_get_int(sym);
        else
            out->fval = st_get_float(sym);
        break;
    }

    case EXPR_BINOP: {
        ExprVal l, r;
        eval_expr(e->u.bin.left,  &l);
        eval_expr(e->u.bin.right, &r);

        /* Relational operators: result is int (0 or 1) */
        if (e->u.bin.op == OP_EQ  || e->u.bin.op == OP_NEQ ||
            e->u.bin.op == OP_LT  || e->u.bin.op == OP_LTE ||
            e->u.bin.op == OP_GT  || e->u.bin.op == OP_GTE) {

            ensure_same_type(&l, &r);
            out->type = TYPE_INT;

            switch (e->u.bin.op) {
            case OP_EQ:  out->fval = (l.fval == r.fval); break;
            case OP_NEQ: out->fval = (l.fval != r.fval); break;
            case OP_LT:  out->fval = (l.fval <  r.fval); break;
            case OP_LTE: out->fval = (l.fval <= r.fval); break;
            case OP_GT:  out->fval = (l.fval >  r.fval); break;
            case OP_GTE: out->fval = (l.fval >= r.fval); break;
            default:
                semantic_error("Invalid relational op");
            }
        }
        /* Arithmetic operators */
        else {
            ensure_same_type(&l, &r);
            out->type = l.type;

            switch (e->u.bin.op) {
            case OP_ADD: out->fval = l.fval + r.fval; break;
            case OP_SUB: out->fval = l.fval - r.fval; break;
            case OP_MUL: out->fval = l.fval * r.fval; break;
            case OP_DIV:
                if (r.fval == 0.0f)
                    semantic_error("Division by zero");
                out->fval = l.fval / r.fval;
                break;
            default:
                semantic_error("Invalid arithmetic op");
            }
        }
        break;
    }
    }
}

/* =======================
   Statement execution
   ======================= */

static void exec_stmt_list(Stmt *s) {
    while (s) {
        exec_stmt(s);
        s = s->next;
    }
}

static void exec_stmt(Stmt *s) {
    if (!s) return;

    switch (s->kind) {

    case STMT_EMPTY:
        /* nothing */
        break;

    case STMT_ASSIGN: {
        ExprVal v;
        eval_expr(s->u.assign.expr, &v);

        Symbol *sym = st_lookup(s->u.assign.name);
        if (sym->type != v.type)
            semantic_error("Type mismatch in assignment to '%s'",
                           s->u.assign.name);

        if (v.type == TYPE_INT)
            st_set_int(sym, (int)v.fval);
        else
            st_set_float(sym, v.fval);
        break;
    }

    case STMT_IF: {
        ExprVal c;
        eval_expr(s->u.sel.cond, &c);

        if (expr_to_bool(&c))
            exec_stmt_list(s->u.sel.then_branch);
        else if (s->u.sel.else_branch)
            exec_stmt_list(s->u.sel.else_branch);
        break;
    }

    case STMT_WHILE: {
        while (1) {
            ExprVal c;
            eval_expr(s->u.wh.cond, &c);
            if (!expr_to_bool(&c))
                break;

            exec_stmt_list(s->u.wh.body);
        }
        break;
    }

    case STMT_BLOCK:
        exec_stmt_list(s->u.block.body);
        break;
    }
}

/* =======================
   Grammar rules (build AST)
   ======================= */

/* program → program ID { declaration-list statement-list } */
static void program_(void) {
    match(T_PROGRAM);
    match(T_ID);
    match(T_LBRACE);

    declaration_list();
    Stmt *body = statement_list();

    match(T_RBRACE);

    /* execute whole program */
    exec_stmt_list(body);
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

    if (lookahead != T_ID)
        syntax_error("identifier");

    char name[64];
    strncpy(name, yytext, sizeof(name) - 1);
    name[sizeof(name) - 1] = '\0';

    match(T_ID);
    match(T_SEMI);

    st_insert(name, t, line);
}

/* type-specifier → int | float */
static VarType type_specifier(void) {
    if (lookahead == T_INT) {
        match(T_INT);
        return TYPE_INT;
    }
    if (lookahead == T_FLOAT) {
        match(T_FLOAT);
        return TYPE_FLOAT;
    }
    syntax_error("type (int or float)");
    return TYPE_INT; /* unreachable */
}

/* statement-list → { statement } */
static Stmt *statement_list(void) {
    Stmt *head = NULL;
    Stmt *tail = NULL;

    while (lookahead == T_ID     ||
           lookahead == T_IF     ||
           lookahead == T_WHILE  ||
           lookahead == T_LBRACE ||
           lookahead == T_SEMI) {

        Stmt *s = statement();

        if (!head) {
            head = tail = s;
        } else {
            tail->next = s;
            while (tail->next)
                tail = tail->next;
        }
    }
    return head;
}

/* statement → assignment-stmt | selection-stmt | iteration-stmt
 *           | compound-stmt | ;
 */
static Stmt *statement(void) {
    if (lookahead == T_ID)     return assignment_stmt();
    if (lookahead == T_IF)     return selection_stmt();
    if (lookahead == T_WHILE)  return iteration_stmt();
    if (lookahead == T_LBRACE) return compound_stmt();
    if (lookahead == T_SEMI) {
        match(T_SEMI);
        return make_empty_stmt();
    }

    syntax_error("statement");
    return NULL;
}

/* compound-stmt → { statement-list } */
static Stmt *compound_stmt(void) {
    match(T_LBRACE);
    Stmt *b = statement_list();
    match(T_RBRACE);
    return make_block_stmt(b);
}

/* assignment-stmt → ID = expression ; */
static Stmt *assignment_stmt(void) {
    char name[64];
    strncpy(name, yytext, sizeof(name) - 1);
    name[sizeof(name) - 1] = '\0';

    match(T_ID);
    match(T_ASSIGN);

    Expr *rhs = expression();
    match(T_SEMI);

    return make_assign_stmt(name, rhs);
}

/* selection-stmt → if ( expression ) statement [ else statement ] */
static Stmt *selection_stmt(void) {
    match(T_IF);
    match(T_LPAREN);

    Expr *cond = expression();

    match(T_RPAREN);

    Stmt *thenb = statement();
    Stmt *elseb = NULL;

    if (lookahead == T_ELSE) {
        match(T_ELSE);
        elseb = statement();
    }

    /* wrap bodies in blocks so exec_stmt_list works uniformly */
    if (thenb && thenb->kind != STMT_BLOCK)
        thenb = make_block_stmt(thenb);
    if (elseb && elseb->kind != STMT_BLOCK)
        elseb = make_block_stmt(elseb);

    return make_if_stmt(cond, thenb, elseb);
}

/* iteration-stmt → while ( expression ) statement */
static Stmt *iteration_stmt(void) {
    match(T_WHILE);
    match(T_LPAREN);

    Expr *cond = expression();

    match(T_RPAREN);

    Stmt *body = statement();

    if (body && body->kind != STMT_BLOCK)
        body = make_block_stmt(body);

    return make_while_stmt(cond, body);
}

/* expression → additive-expression [ relop additive-expression ] */
static Expr *expression(void) {
    Expr *left = additive_expression();

    if (lookahead == T_EQ  || lookahead == T_NEQ ||
        lookahead == T_LT  || lookahead == T_LTE ||
        lookahead == T_GT  || lookahead == T_GTE) {

        int op_tok = lookahead;
        match(lookahead);

        Expr *right = additive_expression();

        if (left->type != right->type)
            semantic_error("Relational type mismatch");

        BinOpKind op;
        switch (op_tok) {
        case T_EQ:  op = OP_EQ;  break;
        case T_NEQ: op = OP_NEQ; break;
        case T_LT:  op = OP_LT;  break;
        case T_LTE: op = OP_LTE; break;
        case T_GT:  op = OP_GT;  break;
        case T_GTE: op = OP_GTE; break;
        default:
            semantic_error("Invalid relational token");
        }

        return make_binop_expr(op, TYPE_INT, left, right);
    }

    return left;
}

/* additive-expression → term { (+|-) term } */
static Expr *additive_expression(void) {
    Expr *left = term();

    while (lookahead == T_PLUS || lookahead == T_MINUS) {
        int op_tok = lookahead;
        match(lookahead);

        Expr *right = term();

        if (left->type != right->type)
            semantic_error("Additive type mismatch");

        BinOpKind op = (op_tok == T_PLUS) ? OP_ADD : OP_SUB;

        left = make_binop_expr(op, left->type, left, right);
    }

    return left;
}

/* term → factor { (*|/) factor } */
static Expr *term(void) {
    Expr *left = factor();

    while (lookahead == T_MULT || lookahead == T_DIV) {
        int op_tok = lookahead;
        match(lookahead);

        Expr *right = factor();

        if (left->type != right->type)
            semantic_error("Term type mismatch");

        BinOpKind op = (op_tok == T_MULT) ? OP_MUL : OP_DIV;

        left = make_binop_expr(op, left->type, left, right);
    }

    return left;
}

/* factor → ( expression ) | ID | NUM */
static Expr *factor(void) {
    if (lookahead == T_LPAREN) {
        match(T_LPAREN);
        Expr *e = expression();
        match(T_RPAREN);
        return e;
    }

    if (lookahead == T_ID) {
        char name[64];
        strncpy(name, yytext, sizeof(name) - 1);
        name[sizeof(name) - 1] = '\0';

        match(T_ID);
        return make_var_expr(name);
    }

    if (lookahead == T_NUM) {
        char buf[64];
        strncpy(buf, yytext, sizeof(buf) - 1);
        buf[sizeof(buf) - 1] = '\0';

        match(T_NUM);

        int is_float = (strchr(buf, '.') != NULL);
        if (is_float)
            return make_num_expr(TYPE_FLOAT, (float)atof(buf));
        else
            return make_num_expr(TYPE_INT, (float)atoi(buf));
    }

    syntax_error("factor");
    return NULL;
}

/* =======================
   main
   ======================= */

int main(int argc, char **argv) {
    if (argc > 1) {
        yyin = fopen(argv[1], "r");
        if (!yyin) {
            fprintf(stderr, "Cannot open %s\n", argv[1]);
            return EXIT_FAILURE;
        }
    }

    st_init();
    advance();
    program_();

    if (lookahead != T_EOF)
        syntax_error("EOF");

    printf("Interpretation completed successfully.\n");
    st_print_all();

    if (yyin && yyin != stdin)
        fclose(yyin);

    return EXIT_SUCCESS;
}
