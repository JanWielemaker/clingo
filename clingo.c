#include <SWI-Stream.h>
#include <SWI-Prolog.h>
#include <cclingo.h>
#include <assert.h>
#include <string.h>

static clingo_module_t *module;

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
	  { Sdprintf("Clingo: %s\n", clingo_error_str(_rc)); \
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

  CLINGO_TRY(clingo_control_ground(ctl, part_span, 0));

  return TRUE;
}



static int
unify_model(term_t t, clingo_model_t *model)
{ return PL_unify_bool(t, TRUE);
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
      { unify_model(Model, model);
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


install_t
install_clingo(void)
{ clingo_module_new(&module);

  PL_register_foreign("clingo_new", 2, pl_clingo_new, 0);
  PL_register_foreign("clingo_add", 2, pl_clingo_add, 0);
  PL_register_foreign("clingo_ground", 2, pl_clingo_ground, 0);
  PL_register_foreign("clingo_solve", 2, pl_clingo_solve, PL_FA_NONDETERMINISTIC);
}
