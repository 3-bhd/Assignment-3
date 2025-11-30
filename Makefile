CC = gcc
CFLAGS = -Wall -Wextra -std=c11

all: cminus_interpreter

lex.yy.c: scanner.l tokens.h
	flex scanner.l

cminus_interpreter: parser.o symbol_table.o lex.yy.o
	$(CC) $(CFLAGS) -o cminus_interpreter parser.o symbol_table.o lex.yy.o

parser.o: parser.c tokens.h symbol_table.h
	$(CC) $(CFLAGS) -c parser.c

symbol_table.o: symbol_table.c symbol_table.h
	$(CC) $(CFLAGS) -c symbol_table.c

lex.yy.o: lex.yy.c tokens.h
	$(CC) $(CFLAGS) -c lex.yy.c

clean:
	rm -f *.o lex.yy.c cminus_interpreter
