#include <clingo.h>
#include <SWI-Prolog.h>

static int unify_ast_term(term_t t, const clingo_ast_term_t *ast);
static int unify_ast_symbol_u_term(term_t t, const clingo_ast_term_t *ast);
static int unify_ast_variable_u_term(term_t t, const clingo_ast_term_t *ast);
static int unify_ast_unary_operation_u_term(term_t t, const clingo_ast_term_t *ast);
static int unify_ast_binary_operation_u_term(term_t t, const clingo_ast_term_t *ast);
static int unify_ast_interval_u_term(term_t t, const clingo_ast_term_t *ast);
static int unify_ast_function_u_term(term_t t, const clingo_ast_term_t *ast);
static int unify_ast_external_function_u_term(term_t t, const clingo_ast_term_t *ast);
static int unify_ast_pool_u_term(term_t t, const clingo_ast_term_t *ast);
static int unify_ast_symbolic_atom(term_t t, const clingo_ast_term_t *ast);
static int unify_ast_literal(term_t t, const clingo_ast_literal_t *ast);
static int unify_ast_comparison_u_literal(term_t t, const clingo_ast_literal_t *ast);
static int unify_ast_symbolic_atom_u_literal(term_t t, const clingo_ast_literal_t *ast);
static int unify_ast_body_literal(term_t t, const clingo_ast_body_literal_t *ast);
static int unify_ast_head_literal(term_t t, const clingo_ast_head_literal_t *ast);
static int unify_ast_statement(term_t t, const clingo_ast_statement_t *ast);
static int unify_ast_rule_u_statement(term_t t, const clingo_ast_statement_t *ast);
static int unify_ast_program_u_statement(term_t t, const clingo_ast_statement_t *ast);
static int unify_ast_id(term_t t, const clingo_ast_id_t *ast);

static int
unify_ast_unary_operator(term_t t, const clingo_ast_unary_operator_t *ep)
{ switch(*ep) {
    case
  }
}


static int
unify_ast_term(term_t t, const clingo_ast_term_t *ast) {
  switch( ast->type ) {
    case clingo_ast_term_type_symbol:
      return unify_ast_symbol_u_term(t, ast);
    case clingo_ast_term_type_variable:
      return unify_ast_variable_u_term(t, ast);
    case clingo_ast_term_type_unary_operation:
      return unify_ast_unary_operation_u_term(t, ast);
    case clingo_ast_term_type_binary_operation:
      return unify_ast_binary_operation_u_term(t, ast);
    case clingo_ast_term_type_interval:
      return unify_ast_interval_u_term(t, ast);
    case clingo_ast_term_type_function:
      return unify_ast_function_u_term(t, ast);
    case clingo_ast_term_type_external_function:
      return unify_ast_external_function_u_term(t, ast);
    case clingo_ast_term_type_pool:
      return unify_ast_pool_u_term(t, ast);
  }
}

static int
unify_ast_symbol_u_term(term_t t, const clingo_ast_term_t *ast) {
  term_t tmp = PL_new_term_ref();
  static functor_t f = 0;

  if ( !f )
    f = PL_new_functor(PL_new_atom("symbol"), 2);
  if ( !PL_unify_functor(t, f) )
    return FALSE;

  if ( !PL_get_arg(1, t, tmp) )
    return FALSE;
  if ( !unify_ast_location(tmp, &ast->location) )
    return FALSE;
  if ( !PL_get_arg(2, t, tmp) )
    return FALSE;
  if ( !unify_ast_symbol(tmp, &ast->symbol) )
    return FALSE;
  return TRUE;
}

static int
unify_ast_variable_u_term(term_t t, const clingo_ast_term_t *ast) {
  term_t tmp = PL_new_term_ref();
  static functor_t f = 0;

  if ( !f )
    f = PL_new_functor(PL_new_atom("variable"), 2);
  if ( !PL_unify_functor(t, f) )
    return FALSE;

  if ( !PL_get_arg(1, t, tmp) )
    return FALSE;
  if ( !unify_ast_location(tmp, &ast->location) )
    return FALSE;
  if ( !PL_get_arg(2, t, tmp) )
    return FALSE;
  if ( !PL_unify_atom_chars(tmp, ast->variable) )
    return FALSE;
  return TRUE;
}

static int
unify_ast_unary_operation_u_term(term_t t, const clingo_ast_term_t *ast) {
  term_t tmp = PL_new_term_ref();
  static functor_t f = 0;

  if ( !f )
    f = PL_new_functor(PL_new_atom("unary_operation"), 3);
  if ( !PL_unify_functor(t, f) )
    return FALSE;

  if ( !PL_get_arg(1, t, tmp) )
    return FALSE;
  if ( !unify_ast_location(tmp, &ast->location) )
    return FALSE;
  if ( !PL_get_arg(2, t, tmp) )
    return FALSE;
  if ( !unify_ast_unary_operator(tmp, &ast->unary_operation->unary_operator) )
    return FALSE;
  if ( !PL_get_arg(3, t, tmp) )
    return FALSE;
  if ( !unify_ast_term(tmp, &ast->unary_operation->argument) )
    return FALSE;
  return TRUE;
}

static int
unify_ast_binary_operation_u_term(term_t t, const clingo_ast_term_t *ast) {
  term_t tmp = PL_new_term_ref();
  static functor_t f = 0;

  if ( !f )
    f = PL_new_functor(PL_new_atom("binary_operation"), 4);
  if ( !PL_unify_functor(t, f) )
    return FALSE;

  if ( !PL_get_arg(1, t, tmp) )
    return FALSE;
  if ( !unify_ast_location(tmp, &ast->location) )
    return FALSE;
  if ( !PL_get_arg(2, t, tmp) )
    return FALSE;
  if ( !unify_ast_binary_operator(tmp, &ast->binary_operation->binary_operator) )
    return FALSE;
  if ( !PL_get_arg(3, t, tmp) )
    return FALSE;
  if ( !unify_ast_term(tmp, &ast->binary_operation->left) )
    return FALSE;
  if ( !PL_get_arg(4, t, tmp) )
    return FALSE;
  if ( !unify_ast_term(tmp, &ast->binary_operation->right) )
    return FALSE;
  return TRUE;
}

static int
unify_ast_interval_u_term(term_t t, const clingo_ast_term_t *ast) {
  term_t tmp = PL_new_term_ref();
  static functor_t f = 0;

  if ( !f )
    f = PL_new_functor(PL_new_atom("interval"), 3);
  if ( !PL_unify_functor(t, f) )
    return FALSE;

  if ( !PL_get_arg(1, t, tmp) )
    return FALSE;
  if ( !unify_ast_location(tmp, &ast->location) )
    return FALSE;
  if ( !PL_get_arg(2, t, tmp) )
    return FALSE;
  if ( !unify_ast_term(tmp, &ast->interval->left) )
    return FALSE;
  if ( !PL_get_arg(3, t, tmp) )
    return FALSE;
  if ( !unify_ast_term(tmp, &ast->interval->right) )
    return FALSE;
  return TRUE;
}

static int
unify_ast_function_u_term(term_t t, const clingo_ast_term_t *ast) {
  term_t tmp = PL_new_term_ref();
  static functor_t f = 0;

  if ( !f )
    f = PL_new_functor(PL_new_atom("function"), 3);
  if ( !PL_unify_functor(t, f) )
    return FALSE;

  if ( !PL_get_arg(1, t, tmp) )
    return FALSE;
  if ( !unify_ast_location(tmp, &ast->location) )
    return FALSE;
  if ( !PL_get_arg(2, t, tmp) )
    return FALSE;
  if ( !PL_unify_atom_chars(tmp, ast->function->name) )
    return FALSE;
  if ( !PL_get_arg(3, t, tmp) )
    return FALSE;
  {
    int i;
    term_t head = PL_copy_term_ref(tmp);
    term_t tail = PL_new_term_ref();

    for(i=0; i<ast->function->size; i++) {
      if ( !PL_unify_list(tail, head, tail) )
        return FALSE;
      if ( !unify_ast_term(head, &ast->function->arguments[i]) )
        return FALSE;
    }
    if ( !PL_unify_nil(tail) )
      return FALSE;
  }
  return TRUE;
}

static int
unify_ast_external_function_u_term(term_t t, const clingo_ast_term_t *ast) {
  term_t tmp = PL_new_term_ref();
  static functor_t f = 0;

  if ( !f )
    f = PL_new_functor(PL_new_atom("external_function"), 3);
  if ( !PL_unify_functor(t, f) )
    return FALSE;

  if ( !PL_get_arg(1, t, tmp) )
    return FALSE;
  if ( !unify_ast_location(tmp, &ast->location) )
    return FALSE;
  if ( !PL_get_arg(2, t, tmp) )
    return FALSE;
  if ( !PL_unify_atom_chars(tmp, ast->external_function->name) )
    return FALSE;
  if ( !PL_get_arg(3, t, tmp) )
    return FALSE;
  {
    int i;
    term_t head = PL_copy_term_ref(tmp);
    term_t tail = PL_new_term_ref();

    for(i=0; i<ast->external_function->size; i++) {
      if ( !PL_unify_list(tail, head, tail) )
        return FALSE;
      if ( !unify_ast_term(head, &ast->external_function->arguments[i]) )
        return FALSE;
    }
    if ( !PL_unify_nil(tail) )
      return FALSE;
  }
  return TRUE;
}

static int
unify_ast_pool_u_term(term_t t, const clingo_ast_term_t *ast) {
  term_t tmp = PL_new_term_ref();
  static functor_t f = 0;

  if ( !f )
    f = PL_new_functor(PL_new_atom("pool"), 2);
  if ( !PL_unify_functor(t, f) )
    return FALSE;

  if ( !PL_get_arg(1, t, tmp) )
    return FALSE;
  if ( !unify_ast_location(tmp, &ast->location) )
    return FALSE;
  if ( !PL_get_arg(2, t, tmp) )
    return FALSE;
  {
    int i;
    term_t head = PL_copy_term_ref(tmp);
    term_t tail = PL_new_term_ref();

    for(i=0; i<ast->pool->size; i++) {
      if ( !PL_unify_list(tail, head, tail) )
        return FALSE;
      if ( !unify_ast_term(head, &ast->pool->arguments[i]) )
        return FALSE;
    }
    if ( !PL_unify_nil(tail) )
      return FALSE;
  }
  return TRUE;
}

static int
unify_ast_symbolic_atom(term_t t, const clingo_ast_term_t *ast) {
  term_t tmp = PL_new_term_ref();
  static functor_t f = 0;

  if ( !f )
    f = PL_new_functor(PL_new_atom("symbolic_atom"), 1);
  if ( !PL_unify_functor(t, f) )
    return FALSE;

  if ( !PL_get_arg(1, t, tmp) )
    return FALSE;
  if ( !unify_ast_term(tmp, ast) )
    return FALSE;
  return TRUE;
}

static int
unify_ast_literal(term_t t, const clingo_ast_literal_t *ast) {
  switch( ast->type ) {
    case clingo_ast_literal_type_comparison:
      return unify_ast_comparison_u_literal(t, ast);
    case clingo_ast_literal_type_symbolic:
      return unify_ast_symbolic_atom_u_literal(t, ast);
  }
}

static int
unify_ast_comparison_u_literal(term_t t, const clingo_ast_literal_t *ast) {
  term_t tmp = PL_new_term_ref();
  static functor_t f = 0;

  if ( !f )
    f = PL_new_functor(PL_new_atom("comparison"), 3);
  if ( !PL_unify_functor(t, f) )
    return FALSE;

  if ( !PL_get_arg(1, t, tmp) )
    return FALSE;
  if ( !unify_ast_comparison_operator(tmp, &ast->comparison->comparison) )
    return FALSE;
  if ( !PL_get_arg(2, t, tmp) )
    return FALSE;
  if ( !unify_ast_term(tmp, &ast->comparison->left) )
    return FALSE;
  if ( !PL_get_arg(3, t, tmp) )
    return FALSE;
  if ( !unify_ast_term(tmp, &ast->comparison->right) )
    return FALSE;
  return TRUE;
}

static int
unify_ast_symbolic_atom_u_literal(term_t t, const clingo_ast_literal_t *ast) {
  term_t tmp = PL_new_term_ref();
  static functor_t f = 0;

  if ( !f )
    f = PL_new_functor(PL_new_atom("symbolic_atom"), 0);
  if ( !PL_unify_functor(t, f) )
    return FALSE;

  return TRUE;
}

static int
unify_ast_body_literal(term_t t, const clingo_ast_body_literal_t *ast) {
  term_t tmp = PL_new_term_ref();
  static functor_t f = 0;

  if ( !f )
    f = PL_new_functor(PL_new_atom("body_literal"), 3);
  if ( !PL_unify_functor(t, f) )
    return FALSE;

  if ( !PL_get_arg(1, t, tmp) )
    return FALSE;
  if ( !unify_ast_location(tmp, &ast->location) )
    return FALSE;
  if ( !PL_get_arg(2, t, tmp) )
    return FALSE;
  if ( !unify_ast_sign(tmp, &ast->sign) )
    return FALSE;
  if ( !PL_get_arg(3, t, tmp) )
    return FALSE;
  if ( !unify_ast_literal(tmp, &ast->literal) )
    return FALSE;
  return TRUE;
}

static int
unify_ast_head_literal(term_t t, const clingo_ast_head_literal_t *ast) {
  term_t tmp = PL_new_term_ref();
  static functor_t f = 0;

  if ( !f )
    f = PL_new_functor(PL_new_atom("head_literal"), 2);
  if ( !PL_unify_functor(t, f) )
    return FALSE;

  if ( !PL_get_arg(1, t, tmp) )
    return FALSE;
  if ( !unify_ast_location(tmp, &ast->location) )
    return FALSE;
  if ( !PL_get_arg(2, t, tmp) )
    return FALSE;
  if ( !unify_ast_literal(tmp, &ast->literal) )
    return FALSE;
  return TRUE;
}

static int
unify_ast_statement(term_t t, const clingo_ast_statement_t *ast) {
  switch( ast->type ) {
    case clingo_ast_statement_type_rule:
      return unify_ast_rule_u_statement(t, ast);
    case clingo_ast_statement_type_program:
      return unify_ast_program_u_statement(t, ast);
  }
}

static int
unify_ast_rule_u_statement(term_t t, const clingo_ast_statement_t *ast) {
  term_t tmp = PL_new_term_ref();
  static functor_t f = 0;

  if ( !f )
    f = PL_new_functor(PL_new_atom("rule"), 3);
  if ( !PL_unify_functor(t, f) )
    return FALSE;

  if ( !PL_get_arg(1, t, tmp) )
    return FALSE;
  if ( !unify_ast_location(tmp, &ast->location) )
    return FALSE;
  if ( !PL_get_arg(2, t, tmp) )
    return FALSE;
  if ( !unify_ast_head_literal(tmp, &ast->rule->head) )
    return FALSE;
  if ( !PL_get_arg(3, t, tmp) )
    return FALSE;
  {
    int i;
    term_t head = PL_copy_term_ref(tmp);
    term_t tail = PL_new_term_ref();

    for(i=0; i<ast->rule->size; i++) {
      if ( !PL_unify_list(tail, head, tail) )
        return FALSE;
      if ( !unify_ast_body_literal(head, &ast->rule->body[i]) )
        return FALSE;
    }
    if ( !PL_unify_nil(tail) )
      return FALSE;
  }
  return TRUE;
}

static int
unify_ast_program_u_statement(term_t t, const clingo_ast_statement_t *ast) {
  term_t tmp = PL_new_term_ref();
  static functor_t f = 0;

  if ( !f )
    f = PL_new_functor(PL_new_atom("program"), 3);
  if ( !PL_unify_functor(t, f) )
    return FALSE;

  if ( !PL_get_arg(1, t, tmp) )
    return FALSE;
  if ( !unify_ast_location(tmp, &ast->location) )
    return FALSE;
  if ( !PL_get_arg(2, t, tmp) )
    return FALSE;
  if ( !PL_unify_atom_chars(tmp, ast->program->name) )
    return FALSE;
  if ( !PL_get_arg(3, t, tmp) )
    return FALSE;
  {
    int i;
    term_t head = PL_copy_term_ref(tmp);
    term_t tail = PL_new_term_ref();

    for(i=0; i<ast->program->size; i++) {
      if ( !PL_unify_list(tail, head, tail) )
        return FALSE;
      if ( !unify_ast_id(head, &ast->program->parameters[i]) )
        return FALSE;
    }
    if ( !PL_unify_nil(tail) )
      return FALSE;
  }
  return TRUE;
}

static int
unify_ast_id(term_t t, const clingo_ast_id_t *ast) {
  term_t tmp = PL_new_term_ref();
  static functor_t f = 0;

  if ( !f )
    f = PL_new_functor(PL_new_atom("id"), 2);
  if ( !PL_unify_functor(t, f) )
    return FALSE;

  if ( !PL_get_arg(1, t, tmp) )
    return FALSE;
  if ( !unify_ast_location(tmp, &ast->location) )
    return FALSE;
  if ( !PL_get_arg(2, t, tmp) )
    return FALSE;
  if ( !PL_unify_atom_chars(tmp, ast->id) )
    return FALSE;
  return TRUE;
}

