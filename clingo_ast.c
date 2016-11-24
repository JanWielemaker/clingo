#include "clingo_ast.h"

static int unify_ast_term(term_t t, const clingo_ast_term_t *ast);
static int unify_ast_symbol_u_term(term_t t, const clingo_ast_term_t *ast);
static int unify_ast_variable_u_term(term_t t, const clingo_ast_term_t *ast);
static int unify_ast_unary_operation_u_term(term_t t, const clingo_ast_term_t *ast);
static int unify_ast_binary_operation_u_term(term_t t, const clingo_ast_term_t *ast);
static int unify_ast_interval_u_term(term_t t, const clingo_ast_term_t *ast);
static int unify_ast_function_u_term(term_t t, const clingo_ast_term_t *ast);
static int unify_ast_external_function_u_term(term_t t, const clingo_ast_term_t *ast);
static int unify_ast_pool_u_term(term_t t, const clingo_ast_term_t *ast);
static int unify_ast_literal(term_t t, const clingo_ast_literal_t *ast);
static int unify_ast_comparison_u_literal(term_t t, const clingo_ast_literal_t *ast);
static int unify_ast_symbolic_atom_u_literal(term_t t, const clingo_ast_literal_t *ast);
static int unify_ast_body_literal(term_t t, const clingo_ast_body_literal_t *ast);
static int unify_ast_head_literal(term_t t, const clingo_ast_head_literal_t *ast);
static int unify_ast_rule_u_statement(term_t t, const clingo_ast_statement_t *ast);
static int unify_ast_program_u_statement(term_t t, const clingo_ast_statement_t *ast);
static int unify_ast_id(term_t t, const clingo_ast_id_t *ast);

static int
unify_ast_sign(term_t t, const clingo_ast_sign_t *ep)
{ switch(*ep) {
    case clingo_ast_sign_none:            { return PL_unify_atom_chars(t, "none"); }
    case clingo_ast_sign_negation:        { return PL_unify_atom_chars(t, "negation"); }
    case clingo_ast_sign_double_negation: { break; }
  }
  return PL_unify_atom_chars(t, "double_negation");
}

static int
unify_ast_unary_operator(term_t t, const clingo_ast_unary_operator_t *ep)
{ switch(*ep) {
    case clingo_ast_unary_operator_minus:    { return PL_unify_atom_chars(t, "minus"); }
    case clingo_ast_unary_operator_absolute: { return PL_unify_atom_chars(t, "absolute"); }
    case clingo_ast_unary_operator_negation: { break; }
  }
  return PL_unify_atom_chars(t, "negation");
}

static int
unify_ast_binary_operator(term_t t, const clingo_ast_binary_operator_t *ep)
{ switch(*ep) {
    case clingo_ast_binary_operator_xor:            { return PL_unify_atom_chars(t, "xor"); }
    case clingo_ast_binary_operator_or:             { return PL_unify_atom_chars(t, "or"); }
    case clingo_ast_binary_operator_and:            { return PL_unify_atom_chars(t, "and"); }
    case clingo_ast_binary_operator_plus:           { return PL_unify_atom_chars(t, "plus"); }
    case clingo_ast_binary_operator_minus:          { return PL_unify_atom_chars(t, "minus"); }
    case clingo_ast_binary_operator_multiplication: { return PL_unify_atom_chars(t, "multiplication"); }
    case clingo_ast_binary_operator_division:       { return PL_unify_atom_chars(t, "division"); }
    case clingo_ast_binary_operator_modulo:         { break; }
  }
  return PL_unify_atom_chars(t, "modulo");
}

static int
unify_ast_comparison_operator(term_t t, const clingo_ast_comparison_operator_t *ep)
{ switch(*ep) {
    case clingo_ast_comparison_operator_greater_than:  { return PL_unify_atom_chars(t, "greater_than"); }
    case clingo_ast_comparison_operator_less_than:     { return PL_unify_atom_chars(t, "less_than"); }
    case clingo_ast_comparison_operator_less_equal:    { return PL_unify_atom_chars(t, "less_equal"); }
    case clingo_ast_comparison_operator_greater_equal: { return PL_unify_atom_chars(t, "greater_equal"); }
    case clingo_ast_comparison_operator_not_equal:     { return PL_unify_atom_chars(t, "not_equal"); }
    case clingo_ast_comparison_operator_equal:         { break; }
  }
  return PL_unify_atom_chars(t, "equal");
}

static int
unify_ast_location(term_t t, const clingo_location_t *loc) {
  return PL_unify_term(t, PL_FUNCTOR_CHARS, "loc", 6,
      PL_CHARS, loc->begin_file,
      PL_INT64, (int64_t)loc->begin_line,
      PL_INT64, (int64_t)loc->begin_column,
      PL_CHARS, loc->end_file,
      PL_INT64, (int64_t)loc->end_line,
      PL_INT64, (int64_t)loc->end_column);
}

static int unify_ast_symbol(term_t t, const clingo_symbol_t *sym) {
  return unify_value(t, *sym);
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
  assert(0);
  return 0;
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
    size_t i;
    term_t tail = PL_copy_term_ref(tmp);
    term_t head = PL_new_term_ref();

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
    size_t i;
    term_t tail = PL_copy_term_ref(tmp);
    term_t head = PL_new_term_ref();

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
    size_t i;
    term_t tail = PL_copy_term_ref(tmp);
    term_t head = PL_new_term_ref();

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
unify_ast_literal(term_t t, const clingo_ast_literal_t *ast) {
  switch( ast->type ) {
    case clingo_ast_literal_type_comparison:
      return unify_ast_comparison_u_literal(t, ast);
    case clingo_ast_literal_type_symbolic:
      return unify_ast_symbolic_atom_u_literal(t, ast);
  }
  assert(0);
  return 0;
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
    f = PL_new_functor(PL_new_atom("symbolic_atom"), 1);
  if ( !PL_unify_functor(t, f) )
    return FALSE;
  if ( !PL_get_arg(1, t, tmp) )
    return FALSE;
  if ( !unify_ast_term(tmp, ast->symbol) )
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
  if ( !unify_ast_literal(tmp, ast->literal) )
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
  if ( !unify_ast_literal(tmp, ast->literal) )
    return FALSE;
  return TRUE;
}

int
unify_ast_statement(term_t t, const clingo_ast_statement_t *ast) {
  switch( ast->type ) {
    case clingo_ast_statement_type_rule:
      return unify_ast_rule_u_statement(t, ast);
    case clingo_ast_statement_type_program:
      return unify_ast_program_u_statement(t, ast);
  }
  assert(FALSE);
  return 0;
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
    size_t i;
    term_t tail = PL_copy_term_ref(tmp);
    term_t head = PL_new_term_ref();

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
    size_t i;
    term_t tail = PL_copy_term_ref(tmp);
    term_t head = PL_new_term_ref();

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

