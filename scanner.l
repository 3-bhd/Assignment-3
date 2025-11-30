%{
#include <stdio.h>
#include "tokens.h"

/* Line counter visible to the parser */
int line = 1;
%}

/* --------- patterns ---------- */
%%
"program"           { return T_PROGRAM; }
"int"               { return T_INT; }
"float"             { return T_FLOAT; }
"if"                { return T_IF; }
"else"              { return T_ELSE; }
"while"             { return T_WHILE; }
"return"            { return T_RETURN; }

/* Relational and assignment operators */
"=="                { return T_EQ; }
"!="                { return T_NEQ; }
"<="                { return T_LTE; }
">="                { return T_GTE; }
"<"                 { return T_LT; }
">"                 { return T_GT; }
"="                 { return T_ASSIGN; }

/* Arithmetic operators */
"+"                 { return T_PLUS; }
"-"                 { return T_MINUS; }
"*"                 { return T_MULT; }
"/"                 { return T_DIV; }

/* Punctuation */
";"                 { return T_SEMI; }
","                 { return T_COMMA; }
"("                 { return T_LPAREN; }
")"                 { return T_RPAREN; }
"{"                 { return T_LBRACE; }
"}"                 { return T_RBRACE; }
"["                 { return T_LBRACKET; }
"]"                 { return T_RBRACKET; }

/* Numbers: we treat both int/float as NUM; parser decides type */
[0-9]+("."[0-9]+)?  { return T_NUM; }

/* Identifiers */
[A-Za-z_][A-Za-z0-9_]*   { return T_ID; }

/* Newlines: update line counter */
"\r\n"              { line++; }     /* Windows */
"\n"                { line++; }     /* Unix   */
"\r"                { line++; }     /* Old Mac */

/* Whitespace (space, tab, formfeed, vertical tab) - skip */
[ \t\v\f]+          { /* skip */ }

/* Any other single character is an error */
.                   {
                        fprintf(stderr,
                                "Lexical error: unknown character '%s' at line %d\n",
                                yytext, line);
                    }

%%

int yywrap(void) {
    return 1;
}
