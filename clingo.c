#define PL_ARITY_AS_SIZE 1
#include <SWI-Stream.h>
#include <SWI-Prolog.h>
#include <pthread.h>
#include <cclingo.h>
#include <assert.h>
#include <string.h>

static clingo_module_t *module;
static atom_t ATOM_inf;
static atom_t ATOM_sup;
static atom_t ATOM_minus;
static atom_t ATOM_hash;
static atom_t ATOM_atoms;
static atom_t ATOM_terms;
static atom_t ATOM_shown;
static atom_t ATOM_csp;
static atom_t ATOM_comp;
static functor_t FUNCTOR_hash1;
static functor_t FUNCTOR_tilde1;
static functor_t FUNCTOR_error2;
static functor_t FUNCTOR_clingo_error1;

static clingo_error_t get_value(term_t t, clingo_value_t *val, int minus);

static clingo_error_t
call_function(char const *, clingo_value_span_t, void *, clingo_value_span_t *);


		 /*******************************
		 *	  SYMBOL WRAPPER	*
		 *******************************/

#define CLINGO_MAGIC 76432248

typedef struct clingo_env
{ clingo_control_t *	control;	/* Underlying stream */
  clingo_value_t       *values;		/* call_function() values */
  pthread_mutex_t	mutex;		/* Avoid concurrent solve */
  int			flags;		/* Misc flags  */
} clingo_env;

typedef struct clingo_wrapper
{ atom_t		symbol;		/* Associated symbol */
  clingo_env	       *clingo;
  int			magic;
} clingo_wrapper;


static pthread_mutex_t clingo_mutex = PTHREAD_MUTEX_INITIALIZER;
#define LOCK()    pthread_mutex_lock(&clingo_mutex);
#define UNLOCK()  pthread_mutex_unlock(&clingo_mutex);

#define LOCK_CONTROL(ctl)   pthread_mutex_lock(&ctl->mutex);
#define UNLOCK_CONTROL(ctl) pthread_mutex_unlock(&ctl->mutex);

static void
acquire_clingo(atom_t symbol)
{ clingo_wrapper *ar = PL_blob_data(symbol, NULL, NULL);
  ar->symbol = symbol;
}


static int
release_clingo(atom_t symbol)
{ clingo_wrapper *ar = PL_blob_data(symbol, NULL, NULL);

  assert(ar->magic == CLINGO_MAGIC);
  if ( ar->clingo )
  { if ( ar->clingo->control )
      clingo_control_free(ar->clingo->control);
    if ( ar->clingo->values )
      free(ar->clingo->values);
    pthread_mutex_destroy(&ar->clingo->mutex);
    PL_free(ar->clingo);
    ar->clingo = NULL;
  }
  PL_free(ar);

  return TRUE;
}

static int
compare_clingos(atom_t a, atom_t b)
{ clingo_wrapper *ara = PL_blob_data(a, NULL, NULL);
  clingo_wrapper *arb = PL_blob_data(b, NULL, NULL);

  return ( ara > arb ?  1 :
	   ara < arb ? -1 : 0
	 );
}

static int
write_clingo(IOSTREAM *s, atom_t symbol, int flags)
{ clingo_wrapper *ar = PL_blob_data(symbol, NULL, NULL);

  Sfprintf(s, "<clingo>(%p)", ar);

  return TRUE;
}

static PL_blob_t clingo_blob =
{ PL_BLOB_MAGIC,
  PL_BLOB_NOCOPY,
  "clingo",
  release_clingo,
  compare_clingos,
  write_clingo,
  acquire_clingo
};


static int
get_clingo(term_t t, clingo_env **ccontrol)
{ PL_blob_t *type;
  void *data;

  if ( PL_get_blob(t, &data, NULL, &type) && type == &clingo_blob)
  { clingo_wrapper *ar = data;

    assert(ar->magic == CLINGO_MAGIC);
    if ( !ar->clingo->control )
      return PL_existence_error("clingo", t);
    *ccontrol = ar->clingo;

    return TRUE;
  }

  return PL_type_error("clingo", t);
}


		 /*******************************
		 *	    PREDICATES		*
		 *******************************/

static int
clingo_status(int rc)
{ if ( rc > 0 )
  { term_t ex;

    if ( (ex=PL_new_term_ref()) &&
	 PL_unify_term(ex, PL_FUNCTOR, FUNCTOR_error2,
		             PL_FUNCTOR, FUNCTOR_clingo_error1,
		               PL_CHARS, clingo_error_str(rc)) )
      return PL_raise_exception(ex);
  }

  return !rc;
}


static foreign_t
pl_clingo_new(term_t ccontrol, term_t options)
{ clingo_control_t *ctl;
  char const *argv[] = { "Clingo", "0", NULL };
  clingo_wrapper *ar;

  clingo_control_new(module, 2, argv, &ctl);
  ar = PL_malloc(sizeof(*ar));
  memset(ar, 0, sizeof(*ar));
  ar->clingo = PL_malloc(sizeof(*ar->clingo));
  memset(ar->clingo, 0, sizeof(*ar->clingo));
  ar->magic = CLINGO_MAGIC;
  ar->clingo->control = ctl;
  pthread_mutex_init(&ar->clingo->mutex, NULL);

  return PL_unify_blob(ccontrol, ar, sizeof(*ar), &clingo_blob);
}


static foreign_t
pl_clingo_close(term_t ccontrol)
{ clingo_env *ctl;

  if ( !get_clingo(ccontrol, &ctl) )
    return FALSE;

  if ( ctl->control )
  { clingo_control_t *c = ctl->control;
    clingo_value_t *v = ctl->values;

    if ( __sync_bool_compare_and_swap(&ctl->control, c, NULL) )
      clingo_control_free(c);
    if ( __sync_bool_compare_and_swap(&ctl->values, v, NULL) )
      free(v);
  }

  return TRUE;
}


static int
get_null_terminated_string(term_t t, char **s, int flags)
{ size_t len;

  if ( PL_get_nchars(t, &len, s, flags|REP_UTF8|CVT_EXCEPTION) )
  { if ( len == strlen(*s) )
      return TRUE;
    return PL_domain_error("null_terminated_string", t);
  }

  return FALSE;
}

#define FAST_PARAMS 10

static foreign_t
pl_clingo_add(term_t ccontrol, term_t params, term_t program)
{ char *prog;
  clingo_env *ctl;
  atom_t name;
  size_t arity;
  char *param_buf[FAST_PARAMS];
  char **prog_params = param_buf;
  term_t arg = PL_new_term_ref();
  int rc;

  if ( !get_clingo(ccontrol, &ctl) )
    return FALSE;

  if ( !PL_get_name_arity(params, &name, &arity) )
    return PL_type_error("callable", params);

  if ( arity+1 > FAST_PARAMS &&
       !(prog_params = malloc(sizeof(char*)*arity)) )
    return PL_resource_error("memory");

  for(size_t i=0; i<arity; i++)
  { _PL_get_arg(i+1, params, arg);
    if ( !get_null_terminated_string(arg, &prog_params[i], CVT_ATOM) )
    { rc = FALSE;
      goto out;
    }
  }
  prog_params[arity] = NULL;

  if ( !get_null_terminated_string(program, &prog,
				   CVT_ATOM|CVT_STRING|CVT_LIST|
				   BUF_DISCARDABLE) )
  { rc = FALSE;
    goto out;
  }

  LOCK();
  rc = clingo_control_add(ctl->control,
			  PL_atom_chars(name),
			  (const char**)prog_params, prog);
  UNLOCK();
  rc = clingo_status(rc);

out:
  if ( prog_params != param_buf )
    free(prog_params);

  return rc;
}


static int
get_params(term_t t, clingo_part_t *pv)
{ atom_t name;
  size_t arity;

  if ( PL_get_name_arity(t, &name, &arity) )
  { term_t arg = PL_new_term_ref();
    clingo_value_t *values;

    if ( !(values = malloc(sizeof(*pv->params.begin)*arity)) )
      return PL_resource_error("memory");

    for(size_t i=0; i<arity; i++)
    { int rc;

      _PL_get_arg(i+1, t, arg);

      LOCK();
      rc = get_value(arg, &values[i], FALSE);
      UNLOCK();
      if ( rc )
      { free(values);
	return clingo_status(rc);
      }
    }

    pv->params.size = arity;
    pv->params.begin = values;
    pv->name = PL_atom_chars(name);

    return TRUE;
  }

  return PL_type_error("callable", t);
}


static foreign_t
pl_clingo_ground(term_t ccontrol, term_t parts)
{ clingo_env *ctl;
  clingo_part_span_t part_span;
  clingo_part_t *part_vec = NULL;
  size_t plen = 0;
  int rc;

  if ( !get_clingo(ccontrol, &ctl) )
    return FALSE;
  switch( PL_skip_list(parts, 0, &plen) )
  { case PL_LIST:
    { term_t tail = PL_copy_term_ref(parts);
      term_t head = PL_new_term_ref();

      if ( !(part_vec = malloc(sizeof(*part_vec)*plen)) )
	return PL_resource_error("memory");
      memset(part_vec, 0, sizeof(*part_vec)*plen);
      for(size_t i=0; PL_get_list(tail, head, tail); i++)
      { if ( !get_params(head, &part_vec[i]) )
	{ rc = FALSE;
	  goto out;
	}
      }
      break;
    }
    default:
      return PL_type_error("list", parts);
  }

  part_span.begin = part_vec;
  part_span.size = plen;

  LOCK();
  rc = clingo_control_ground(ctl->control, part_span, call_function, ctl);
  UNLOCK();
  if ( ctl->values )
  { free(ctl->values);
    ctl->values = NULL;
  }
  rc = clingo_status(rc);

out:
  if ( part_vec )
  { for(size_t i; i<plen; i++)
      free((void*)part_vec[i].params.begin);
    free(part_vec);
  }

  return rc;
}


static foreign_t
pl_clingo_assign_external(term_t ccontrol, term_t Atom, term_t Value)
{ clingo_env *ctl;
  clingo_value_t atom;
  clingo_truth_value_t value;
  int bv, rc;

  if ( !get_clingo(ccontrol, &ctl) )
    return FALSE;
  LOCK();
  rc = get_value(Atom, &atom, FALSE);
  UNLOCK();
  if ( !(rc = clingo_status(rc)) )
    return FALSE;
  if ( PL_is_variable(Value) )
    value = clingo_truth_value_free;
  else if ( PL_get_bool_ex(Value, &bv) )
    value = bv ? clingo_truth_value_true : clingo_truth_value_false;

  return clingo_status(
	     clingo_control_assign_external(ctl->control, atom, value));
}


static foreign_t
pl_clingo_release_external(term_t ccontrol, term_t Atom)
{ clingo_env *ctl;
  clingo_value_t atom;
  int rc;

  if ( !get_clingo(ccontrol, &ctl) )
    return FALSE;

  LOCK();
  rc = get_value(Atom, &atom, FALSE);
  UNLOCK();
  if ( !(rc=clingo_status(rc)) )
    return FALSE;

  return clingo_status(clingo_control_release_external(ctl->control, atom));
}


static int
unify_value(term_t t, clingo_value_t v)
{ switch( clingo_value_type(v) )
  { case clingo_value_type_num:
      return PL_unify_integer(t, clingo_value_num(v));
    case clingo_value_type_str:
      return PL_unify_chars(t, PL_STRING|REP_UTF8, (size_t)-1,
			    clingo_value_str(v));
    case clingo_value_type_id:
      return PL_unify_chars(t, PL_ATOM|REP_UTF8, (size_t)-1,
			    clingo_value_name(v));
    case clingo_value_type_inf:
      return PL_unify_term(t, PL_FUNCTOR, FUNCTOR_hash1,
			   PL_ATOM, ATOM_inf);
    case clingo_value_type_sup:
      return PL_unify_term(t, PL_FUNCTOR, FUNCTOR_hash1,
			   PL_ATOM, ATOM_sup);
    case clingo_value_type_fun:
    { atom_t name = PL_new_atom(clingo_value_name(v));
      clingo_value_span_t args = clingo_value_args(v);

      if ( PL_unify_functor(t, PL_new_functor(name, args.size)) )
      { term_t arg = PL_new_term_ref();
	clingo_value_t const *it, *ie;
	int i;

	PL_unregister_atom(name);
	for(i=1, it = args.begin, ie = it + args.size; it != ie; ++it, i++)
	{ _PL_get_arg(i, t, arg);
	  if ( !unify_value(arg, *it) )
	    return FALSE;
	}

	return TRUE;
      }

      return FALSE;
    }
    default:
      assert(0);
      return FALSE;
  }
}

static int
unify_list_from_span(term_t list, clingo_value_span_t *span)
{ term_t tail = PL_copy_term_ref(list);
  term_t head = PL_new_term_ref();
  term_t tmp = PL_new_term_ref();
  clingo_value_t const *it, *ie;

  for (it = span->begin, ie = it + span->size; it != ie; ++it)
  { PL_put_variable(tmp);

    if ( !unify_value(tmp, *it) ||
	 !PL_unify_list(tail, head, tail) ||
	 !PL_unify(head, tmp) )
    { return FALSE;
    }
  }

  return PL_unify_nil(tail);
}


static int
unify_model(term_t t, int show, clingo_model_t *model)
{ clingo_value_span_t atoms;

  if ( !clingo_status(clingo_model_atoms(model, show, &atoms)) )
    return FALSE;

  return unify_list_from_span(t, &atoms);
}


static int
get_assumption(term_t t, clingo_symbolic_literal_t *assump)
{ int rc;

  if ( PL_is_functor(t, FUNCTOR_tilde1) )
  { _PL_get_arg(1, t, t);
    assump->sign = TRUE;
  } else
    assump->sign = FALSE;

  LOCK();
  rc = get_value(t, &assump->atom, FALSE);
  UNLOCK();

  return rc;
}


static int
get_show_map(term_t t, int *map)
{ term_t tail = PL_copy_term_ref(t);
  term_t head = PL_new_term_ref();

  *map = 0;
  while(PL_get_list(tail,head,tail))
  { atom_t a;

    if ( PL_get_atom_ex(head, &a) )
    { if ( a == ATOM_atoms )
	*map |= clingo_show_type_atoms;
      else if ( a == ATOM_terms )
	*map |= clingo_show_type_terms;
      else if ( a == ATOM_shown )
	*map |= clingo_show_type_shown;
      else if ( a == ATOM_csp )
	*map |= clingo_show_type_csp;
      else if ( a == ATOM_comp )
	*map |= clingo_show_type_comp;
      else
	return PL_domain_error("clingo_show", head);
    } else
      return FALSE;
  }

  return PL_get_nil_ex(tail);
}


typedef struct solve_state
{ clingo_env *ctl;
  clingo_solve_iter_t *it;
  int locked;
} solve_state;

static foreign_t
pl_clingo_solve(term_t ccontrol,
		term_t assumptions, term_t Show,
		term_t Model, control_t h)
{ solve_state state_buf;
  solve_state *state = &state_buf;

  switch( PL_foreign_control(h) )
  { case PL_FIRST_CALL:
    { clingo_model_t *model;
      clingo_symbolic_literal_span_t assump_span;
      clingo_symbolic_literal_t *assump_vec = NULL;
      size_t alen = 0;
      int rc;

      memset(state, 0, sizeof(*state));
      if ( !get_clingo(ccontrol, &state->ctl) )
	return FALSE;

      switch(PL_skip_list(assumptions, 0, &alen))
      { case PL_LIST:
	{ term_t tail = PL_copy_term_ref(assumptions);
	  term_t head = PL_new_term_ref();

	  if ( !(assump_vec = malloc(sizeof(*assump_vec)*alen)) )
	    return PL_resource_error("memory");
	  memset(assump_vec, 0, sizeof(*assump_vec)*alen);
	  for(size_t i=0; PL_get_list(tail, head, tail); i++)
	  { int crc;

	    if ( (crc=get_assumption(head, &assump_vec[i])) )
	    { rc = clingo_status(crc);
	      goto out;
	    }
	  }
	  break;
	}
        default:
	  rc = PL_type_error("list", assumptions);
	  goto out;
      }

      assump_span.size = alen;
      assump_span.begin = assump_vec;
      LOCK_CONTROL(state->ctl);
      state->locked = TRUE;
      rc = clingo_control_solve_iter(state->ctl->control,
				     &assump_span, &state->it);
      rc = clingo_status(rc);

    out:
      if ( assump_vec )
	free(assump_vec);
      if ( !rc )
      { out_false:
	if ( state->locked )
	  UNLOCK_CONTROL(state->ctl);
	if ( state != &state_buf )
	  free(state);
	return FALSE;
      }
    next:
      rc = clingo_status(clingo_solve_iter_next(state->it, &model));
      if ( rc && model )
      { int show;

	if ( !get_show_map(Show, &show) )
	  goto out_false;

	if ( !unify_model(Model, show, model) )
	{ if ( PL_exception(0) )
	    goto out_false;
	  goto next;
	}
	if ( state == &state_buf )
	{ if ( !(state = malloc(sizeof(*state))) )
	  { state = &state_buf;
	    PL_resource_error("memory");
	    goto out_false;
	  }
	  *state = state_buf;
	}
	PL_retry_address(state);
      } else
      { clingo_solve_iter_close(state->it);
	goto out_false;
      }
    }
    case PL_REDO:
    { state = PL_foreign_context_address(h);
      goto next;
    }
    case PL_PRUNED:
    default:
    { state = PL_foreign_context_address(h);
      clingo_solve_iter_close(state->it);
      UNLOCK_CONTROL(state->ctl);
      free(state);
      return TRUE;
    }
  }
}


		 /*******************************
		 *	     CALLBACK		*
		 *******************************/

static clingo_error_t
get_value(term_t t, clingo_value_t *val, int minus)
{ switch(PL_term_type(t))
  { case PL_INTEGER:
    { int i;

      if ( PL_get_integer(t, &i) )
      { clingo_value_new_num(i, val);
	return 0;
      }
      return -1;
    }
    case PL_ATOM:
    { char *s;
      size_t len;

      if ( PL_get_nchars(t, &len, &s, CVT_ATOM|REP_UTF8|CVT_EXCEPTION) )
	return clingo_value_new_id(s, minus, val); /* no sign */
      return -1;
    }
    case PL_STRING:
    { char *s;
      size_t len;

      if ( PL_get_nchars(t, &len, &s, CVT_STRING|REP_UTF8|CVT_EXCEPTION) )
	return clingo_value_new_str(s, val);
      return -1;
    }
    case PL_TERM:
    { atom_t name;
      size_t arity;				/* TBD: -atom, #const */

      if ( PL_get_name_arity(t, &name, &arity) )
      { term_t arg = PL_new_term_ref();

	if ( name == ATOM_minus && arity == 1 )
	{ return get_value(arg, val, TRUE);
	} else if ( name == ATOM_hash && arity == 1 )
	{ atom_t a;

	  _PL_get_arg(1, t, arg);
	  if ( PL_get_atom_ex(arg, &a) )
	  { if ( a == ATOM_inf )
	    { clingo_value_new_inf(val);
	      return 0;
	    } else if ( a == ATOM_sup )
	    { clingo_value_new_sup(val);
	      return 0;
	    } else
	      PL_domain_error("clingo_keyword", arg);
	  }

	  return -1;
	} else
	{ clingo_value_span_t span;
	  const char *id = PL_atom_chars(name);		/* TBD: errors */
	  clingo_value_t *values;
	  int rc, i;

	  if ( !(values = malloc(sizeof(*span.begin)*arity)) )
	    return clingo_error_bad_alloc;

	  for(i=0; i<arity; i++)
	  { _PL_get_arg(i+1, t, arg);
	    if ( (rc=get_value(arg, &values[i], FALSE)) != 0 )
	    { free(values);
	      return rc;
	    }
	  }
	  PL_reset_term_refs(arg);

	  span.size = arity;
	  span.begin = values;
	  rc = clingo_value_new_fun(id, span, minus, val);
	  free(values);

	  return rc;
	}
      }

      return -1;
    }
    default:
      PL_type_error("clingo_value", t);
      return -1;
  }
}

/* MT: grounding is already locked */

static clingo_error_t
call_function(char const *name,
	      clingo_value_span_t in,
	      void *closure,
	      clingo_value_span_t *out)
{ static predicate_t pred = 0;
  fid_t fid = 0;
  qid_t qid = 0;
  clingo_error_t rc;
  clingo_env *ctl = closure;

  if ( !pred )
    pred = PL_predicate("inject_values", 3, "clingo");

  if ( ctl->values )
  { free(ctl->values);
    ctl->values = NULL;
  }

  if ( (fid = PL_open_foreign_frame()) )
  { term_t av = PL_new_term_refs(3);
    size_t allocated = 0;
    size_t count = 0;
    clingo_value_t *values = NULL;

    PL_put_atom_chars(av+0, name);
    unify_list_from_span(av+1, &in);
    if ( (qid=PL_open_query(NULL, PL_Q_PASS_EXCEPTION, pred, av)) )
    { while(PL_next_solution(qid))
      { if ( count+1 > allocated )
	{ clingo_value_t *new;
	  allocated = allocated ? allocated*2 : 32;
	  if ( (new=realloc(values, sizeof(*values)*allocated)) )
	  { values = new;
	  } else
	  { free(values);
	    PL_resource_error("memory");
	    rc = -1;
	    goto error;
	  }
	}

	if ( (rc=get_value(av+2, &values[count++], FALSE)) )
	  goto error;
      }
      if ( PL_exception(0) )
      { free(values);
	rc = -1;
	goto error;
      }
      PL_close_query(qid);
    }
    PL_close_foreign_frame(fid);

    out->size = count;
    out->begin = values;
    ctl->values = values;

    return 0;
  }

error:
  if ( qid )
    PL_close_query(qid);
  if ( fid )
    PL_close_foreign_frame(fid);

  return rc;
}


install_t
install_clingo(void)
{ clingo_module_new(&module);

  ATOM_sup = PL_new_atom("sup");
  ATOM_inf = PL_new_atom("inf");
  ATOM_minus = PL_new_atom("-");
  ATOM_hash = PL_new_atom("#");
  ATOM_atoms = PL_new_atom("atoms");
  ATOM_terms = PL_new_atom("terms");
  ATOM_shown = PL_new_atom("shown");
  ATOM_csp = PL_new_atom("csp");
  ATOM_comp = PL_new_atom("comp");
  FUNCTOR_hash1 = PL_new_functor(ATOM_hash, 1);
  FUNCTOR_tilde1 = PL_new_functor(PL_new_atom("~"), 1);
  FUNCTOR_clingo_error1 = PL_new_functor(PL_new_atom("clingo_error"), 1);
  FUNCTOR_error2 = PL_new_functor(PL_new_atom("error"), 2);

  PL_register_foreign("clingo_new", 2, pl_clingo_new, 0);
  PL_register_foreign("clingo_close", 1, pl_clingo_close, 0);
  PL_register_foreign("clingo_add", 3, pl_clingo_add, 0);
  PL_register_foreign("clingo_ground", 2, pl_clingo_ground, 0);
  PL_register_foreign("clingo_solve", 4, pl_clingo_solve, PL_FA_NONDETERMINISTIC);
  PL_register_foreign("clingo_assign_external", 3, pl_clingo_assign_external, 0);
  PL_register_foreign("clingo_release_external", 2, pl_clingo_release_external, 0);
}
