#define PL_ARITY_AS_SIZE 1
#include <SWI-Stream.h>
#include <SWI-Prolog.h>
#include <cclingo.h>
#include <assert.h>
#include <string.h>

static clingo_module_t *module;
static atom_t ATOM_inf;
static atom_t ATOM_sup;
static atom_t ATOM_minus;
static atom_t ATOM_hash;
static functor_t FUNCTOR_hash1;

static bool_t
exists_function(const char *name)
{ return TRUE;
}

static clingo_error_t
call_function(char const *, clingo_value_span_t, clingo_value_span_t *);

static clingo_context_t clingo_context =
{ exists_function,
  call_function
};

		 /*******************************
		 *	  SYMBOL WRAPPER	*
		 *******************************/

#define CLINGO_MAGIC 76432248

typedef struct clingo_wrapper
{ atom_t		symbol;		/* Associated symbol */
  clingo_control_t *	control;	/* Underlying stream */
  int			magic;
} clingo_wrapper;


static void
acquire_clingo(atom_t symbol)
{ clingo_wrapper *ar = PL_blob_data(symbol, NULL, NULL);
  ar->symbol = symbol;
}


static int
release_clingo(atom_t symbol)
{ clingo_wrapper *ar = PL_blob_data(symbol, NULL, NULL);

  assert(ar->magic == CLINGO_MAGIC);
  clingo_control_free(ar->control);
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
get_clingo(term_t t, clingo_control_t **ccontrol)
{ PL_blob_t *type;
  void *data;

  if ( PL_get_blob(t, &data, NULL, &type) && type == &clingo_blob)
  { clingo_wrapper *ar = data;

    assert(ar->magic == CLINGO_MAGIC);
    *ccontrol = ar->control;

    return TRUE;
  }

  return PL_type_error("clingo", t);
}


		 /*******************************
		 *	    PREDICATES		*
		 *******************************/

#define CLINGO_TRY(g) \
	{ int _rc = (g); \
	  if ( _rc != 0 ) \
	  { if ( _rc > 0 ) \
	      Sdprintf("Clingo: %s\n", clingo_error_str(_rc)); \
	    return FALSE; \
	  } \
	}

static foreign_t
pl_clingo_new(term_t ccontrol, term_t options)
{ clingo_control_t *ctl;
  char const *argv[] = { "Clingo", "0", NULL };
  clingo_wrapper *ar;

  clingo_control_new(module, 2, argv, &ctl);
  ar = PL_malloc(sizeof(*ar));
  memset(ar, 0, sizeof(*ar));
  ar->magic = CLINGO_MAGIC;
  ar->control = ctl;

  return PL_unify_blob(ccontrol, ar, sizeof(*ar), &clingo_blob);
}


static foreign_t
pl_clingo_add(term_t ccontrol, term_t program)
{ char *s;
  size_t len;
  clingo_control_t *ctl;

  if ( get_clingo(ccontrol, &ctl) &&
       PL_get_nchars(program, &len, &s,
		     CVT_ATOM|CVT_STRING|CVT_LIST|REP_UTF8|
		     CVT_EXCEPTION|BUF_DISCARDABLE) )
  { char const *base_params[] = { 0 };

    CLINGO_TRY(clingo_control_add(ctl, "base", base_params, s));
    return TRUE;
  }

  return FALSE;
}


static foreign_t
pl_clingo_ground(term_t ccontrol, term_t options)
{ clingo_control_t *ctl;
  clingo_value_t empty[] = {};
  clingo_part_t part_vec[] = { {"base", { empty, 0 } } };
  clingo_part_span_t part_span = { part_vec, 1 };

  if ( !get_clingo(ccontrol, &ctl) )
    return FALSE;

  CLINGO_TRY(clingo_control_ground(ctl, part_span, &clingo_context));

  return TRUE;
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
unify_model(term_t t, clingo_model_t *model)
{ clingo_value_span_t atoms;
  int rc;

  CLINGO_TRY(clingo_model_atoms(model, clingo_show_type_atoms, &atoms));
  rc = unify_list_from_span(t, &atoms);
  clingo_free((void*)atoms.begin);

  return rc;
}


static foreign_t
pl_clingo_solve(term_t ccontrol, term_t Model, control_t h)
{ clingo_solve_iter_t *it;

  switch( PL_foreign_control(h) )
  { case PL_FIRST_CALL:
    { clingo_control_t *ctl;
      clingo_model_t *model;

      if ( !get_clingo(ccontrol, &ctl) )
	return FALSE;

      CLINGO_TRY(clingo_control_solve_iter(ctl, &it));
    next:
      CLINGO_TRY(clingo_solve_iter_next(it, &model));
      if ( model )
      { if ( !unify_model(Model, model) )
	{ if ( PL_exception(0) )
	    return FALSE;
	  goto next;
	}
	PL_retry_address(it);
      } else
      { clingo_solve_iter_close(it);
	return FALSE;
      }
    }
    case PL_REDO:
    { it = PL_foreign_context_address(h);
      goto next;
    }
    case PL_PRUNED:
    { it = PL_foreign_context_address(h);
      clingo_solve_iter_close(it);
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


static clingo_error_t
call_function(char const *name,
	      clingo_value_span_t in,
	      clingo_value_span_t *out)
{ static predicate_t pred = 0;
  fid_t fid = 0;
  qid_t qid = 0;
  clingo_error_t rc;

  if ( !pred )
    pred = PL_predicate("inject_values", 3, "clingo");

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
      { rc = -1;
	goto error;
      }
      PL_close_query(qid);
    }
    PL_close_foreign_frame(fid);

    out->size = count;
    out->begin = values;

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
  FUNCTOR_hash1 = PL_new_functor(ATOM_hash, 1);

  PL_register_foreign("clingo_new", 2, pl_clingo_new, 0);
  PL_register_foreign("clingo_add", 2, pl_clingo_add, 0);
  PL_register_foreign("clingo_ground", 2, pl_clingo_ground, 0);
  PL_register_foreign("clingo_solve", 2, pl_clingo_solve, PL_FA_NONDETERMINISTIC);
}
