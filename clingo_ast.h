#include <clingo.h>
#include <SWI-Prolog.h>
#include <assert.h>

int unify_ast_statement(term_t t, const clingo_ast_statement_t *ast);
int unify_value(term_t t, clingo_symbol_t v);
