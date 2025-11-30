# C- Interpreter (Assignment 3)

This folder contains a simple single-pass interpreter for the C- language subset used in CSCE 4101.

## Files

- `tokens.h`          — token definitions shared by scanner and parser.
- `scanner.l`         — Flex scanner specification.
- `symbol_table.h`    — symbol table interface and type definitions.
- `symbol_table.c`    — symbol table implementation and semantic error helper.
- `parser.c`          — recursive-descent parser + on-the-fly interpreter.
- `Makefile`          — build rules.

## Build

```bash
make
