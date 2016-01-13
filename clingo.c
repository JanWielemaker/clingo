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

static foreign_t
pl_clingo_control_new(term_t ccontrol, term_t options)
{ clingo_control_t *cct;
  char const *argv[] = { "Clingo", NULL };
  clingo_wrapper *ar;

  clingo_control_new(module, 1, argv, &cct);
  ar = PL_malloc(sizeof(*ar));
  memset(ar, 0, sizeof(*ar));
  ar->magic = CLINGO_MAGIC;
  ar->control = cct;

  return PL_unify_blob(ccontrol, ar, sizeof(*ar), &clingo_blob);
}


install_t
install_clingo(void)
{ clingo_module_new(&module);

  PL_register_foreign("clingo_control_new", 2, pl_clingo_control_new, 0);
}
