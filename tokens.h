#ifndef TOKENS_H
#define TOKENS_H

/* Token type enumeration for the C- language */
enum Token {
    T_EOF = 0,
    T_PROGRAM,
    T_INT,
    T_FLOAT,
    T_IF,
    T_ELSE,
    T_WHILE,
    T_RETURN,
    T_ID,
    T_NUM,
    T_PLUS,
    T_MINUS,
    T_MULT,
    T_DIV,
    T_ASSIGN,   /* = */
    T_EQ,       /* == */
    T_NEQ,      /* != */
    T_LT,       /* <  */
    T_LTE,      /* <= */
    T_GT,       /* >  */
    T_GTE,      /* >= */
    T_SEMI,     /* ;  */
    T_COMMA,    /* ,  */
    T_LPAREN,   /* (  */
    T_RPAREN,   /* )  */
    T_LBRACE,   /* {  */
    T_RBRACE,   /* }  */
    T_LBRACKET, /* [  */
    T_RBRACKET  /* ]  */
};

/* Helper to print token names in error messages */
static const char *token_name(int t) {
    switch (t) {
        case T_EOF:       return "EOF";
        case T_PROGRAM:   return "program";
        case T_INT:       return "int";
        case T_FLOAT:     return "float";
        case T_IF:        return "if";
        case T_ELSE:      return "else";
        case T_WHILE:     return "while";
        case T_RETURN:    return "return";
        case T_ID:        return "identifier";
        case T_NUM:       return "number";
        case T_PLUS:      return "+";
        case T_MINUS:     return "-";
        case T_MULT:      return "*";
        case T_DIV:       return "/";
        case T_ASSIGN:    return "=";
        case T_EQ:        return "==";
        case T_NEQ:       return "!=";
        case T_LT:        return "<";
        case T_LTE:       return "<=";
        case T_GT:        return ">";
        case T_GTE:       return ">=";
        case T_SEMI:      return ";";
        case T_COMMA:     return ",";
        case T_LPAREN:    return "(";
        case T_RPAREN:    return ")";
        case T_LBRACE:    return "{";
        case T_RBRACE:    return "}";
        case T_LBRACKET:  return "[";
        case T_RBRACKET:  return "]";
        default:          return "unknown token";
    }
}

#endif /* TOKENS_H */
