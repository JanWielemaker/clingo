#include <SWI-Stream.h>
#include <SWI-Prolog.h>
#include <assert.h>
#include <clingo.h>
#include <pthread.h>
#include <stdio.h>
#include <string.h>

#define FAST_PARAMS 10

static atom_t ATOM_inf;
static atom_t ATOM_sup;
static atom_t ATOM_minus;
static atom_t ATOM_hash;
static atom_t ATOM_atoms;
static atom_t ATOM_terms;
static atom_t ATOM_shown;
static atom_t ATOM_csp;
static atom_t ATOM_comp;
static atom_t ATOM_fact;
static atom_t ATOM_external;
static atom_t ATOM_unknown;
static functor_t FUNCTOR_hash1;
static functor_t FUNCTOR_tilde1;
static functor_t FUNCTOR_error2;
static functor_t FUNCTOR_clingo_error1;
static functor_t FUNCTOR_symbol2;
static functor_t FUNCTOR_symbolic_atoms2;

static bool get_value(term_t t, clingo_symbol_t *val, int minus);

static bool call_function(clingo_location_t const *, char const *,
                          clingo_symbol_t const *, size_t, void *,
                          clingo_symbol_callback_t, void *);

#ifndef PL_ARITY_AS_SIZE
int get_name_arity(term_t t, atom_t *name, size_t *arity) {
    int ret, rc;
    rc = PL_get_name_arity(t, name, &ret);
    *arity = ret;
    return rc;
}
#else
#define get_name_arity PL_get_name_arity
#endif

////////////////////////////// SYMBOL WRAPPER //////////////////////////////

#define CLINGO_MAGIC 76432248

typedef struct clingo_env {
    clingo_control_t *control; /* Underlying stream */
    int flags;                 /* Misc flags  */
} clingo_env;

typedef struct clingo_wrapper {
    atom_t symbol; /* Associated symbol */
    clingo_env *clingo;
    int magic;
} clingo_wrapper;

static void acquire_clingo(atom_t symbol) {
    clingo_wrapper *ar = PL_blob_data(symbol, NULL, NULL);
    ar->symbol = symbol;
}

static int release_clingo(atom_t symbol) {
    clingo_wrapper *ar = PL_blob_data(symbol, NULL, NULL);

    assert(ar->magic == CLINGO_MAGIC);
    if (ar->clingo) {
        if (ar->clingo->control) {
            clingo_control_free(ar->clingo->control);
        }
        PL_free(ar->clingo);
        ar->clingo = NULL;
    }
    PL_free(ar);

    return TRUE;
}

static int compare_clingos(atom_t a, atom_t b) {
    clingo_wrapper *ara = PL_blob_data(a, NULL, NULL);
    clingo_wrapper *arb = PL_blob_data(b, NULL, NULL);

    return (ara > arb ? 1 : ara < arb ? -1 : 0);
}

static int write_clingo(IOSTREAM *s, atom_t symbol, int flags) {
    (void)flags;
    clingo_wrapper *ar = PL_blob_data(symbol, NULL, NULL);

    Sfprintf(s, "<clingo>(%p)", ar);

    return TRUE;
}

#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wmissing-field-initializers"

static PL_blob_t clingo_blob = {PL_BLOB_MAGIC,  PL_BLOB_NOCOPY,  "clingo",
                                release_clingo, compare_clingos, write_clingo,
                                acquire_clingo};
#pragma GCC diagnostic pop

static int get_clingo(term_t t, clingo_env **ccontrol) {
    PL_blob_t *type;
    void *data;

    if (PL_get_blob(t, &data, NULL, &type) && type == &clingo_blob) {
        clingo_wrapper *ar = data;

        assert(ar->magic == CLINGO_MAGIC);
        if (!ar->clingo->control) {
            return PL_existence_error("clingo", t);
        }
        *ccontrol = ar->clingo;

        return TRUE;
    }

    return PL_type_error("clingo", t);
}

////////////////////////////// PREDICATES //////////////////////////////

static bool clingo_status(bool ret) {
    if (!ret) {
        term_t ex;

        if ((ex = PL_new_term_ref()) &&
            PL_unify_term(ex, PL_FUNCTOR, FUNCTOR_error2, PL_FUNCTOR,
                          FUNCTOR_clingo_error1, PL_CHARS,
                          clingo_error_message(), PL_VARIABLE)) {
            return PL_raise_exception(ex);
        }
    }

    return ret;
}

static foreign_t pl_clingo_new(term_t ccontrol, term_t options) {
    (void)options;
    clingo_control_t *ctl;
    char const *argv[] = {"-n", "0"};
    clingo_wrapper *ar;

    // TODO: no error checking here?
    // TODO: might take a logger for handling info messages
    clingo_control_new(argv, 2, NULL, NULL, 20, &ctl);

    ar = PL_malloc(sizeof(*ar));
    memset(ar, 0, sizeof(*ar));
    ar->clingo = PL_malloc(sizeof(*ar->clingo));
    memset(ar->clingo, 0, sizeof(*ar->clingo));
    ar->magic = CLINGO_MAGIC;
    ar->clingo->control = ctl;

    return PL_unify_blob(ccontrol, ar, sizeof(*ar), &clingo_blob);
}

static foreign_t pl_clingo_close(term_t ccontrol) {
    clingo_env *ctl;

    if (!get_clingo(ccontrol, &ctl)) {
        return FALSE;
    }

    if (ctl->control) {
        clingo_control_t *c = ctl->control;

        if (__sync_bool_compare_and_swap(&ctl->control, c, NULL)) {
            clingo_control_free(c);
        }
    }

    return TRUE;
}

static int get_null_terminated_string(term_t t, char **s, int flags) {
    size_t len;

    if (PL_get_nchars(t, &len, s, flags | REP_UTF8 | CVT_EXCEPTION)) {
        if (len == strlen(*s)) {
            return TRUE;
        }
        return PL_domain_error("null_terminated_string", t);
    }

    return FALSE;
}

static foreign_t pl_clingo_add(term_t ccontrol, term_t params, term_t program) {
    char *prog;
    clingo_env *ctl;
    atom_t name;
    size_t arity;
    char *param_buf[FAST_PARAMS];
    char **prog_params = param_buf;
    term_t arg = PL_new_term_ref();
    int rc;

    if (!(rc = get_clingo(ccontrol, &ctl))) {
        goto out;
    }

    if (!get_name_arity(params, &name, &arity)) {
        rc = PL_type_error("callable", params);
        goto out;
    }

    if (arity + 1 > FAST_PARAMS &&
        !(prog_params = malloc(sizeof(char *) * arity))) {
        rc = PL_resource_error("memory");
        goto out;
    }

    for (size_t i = 0; i < arity; i++) {
        _PL_get_arg(i + 1, params, arg);
        if (!(rc =
                  get_null_terminated_string(arg, &prog_params[i], CVT_ATOM))) {
            goto out;
        }
    }
    if (!(rc = get_null_terminated_string(program, &prog,
                                          CVT_ATOM | CVT_STRING | CVT_LIST |
                                              BUF_DISCARDABLE))) {
        goto out;
    }
    if (!(rc = clingo_status(
              clingo_control_add(ctl->control, PL_atom_chars(name),
                                 (const char **)prog_params, arity, prog)))) {
        goto out;
    }

out:
    if (prog_params != param_buf) {
        free(prog_params);
    }

    return rc;
}

static int get_params(term_t t, clingo_part_t *pv) {
    int rc;
    atom_t name;
    term_t arg;
    clingo_symbol_t *values = NULL;

    if (!(rc = get_name_arity(t, &name, &pv->size))) {
        rc = PL_type_error("callable", t);
        goto out;
    }

    arg = PL_new_term_ref();

    if (!(values = malloc(sizeof(*pv->params) * pv->size))) {
        rc = PL_resource_error("memory");
        goto out;
    }

    for (size_t i = 0; i < pv->size; i++) {
        _PL_get_arg(i + 1, t, arg);
        if (!(rc = clingo_status(get_value(arg, &values[i], FALSE)))) {
            goto out;
        }
    }

    pv->params = values;
    pv->name = PL_atom_chars(name);
    values = NULL;

out:
    if (values) {
        free(values);
    }
    return rc;
}

static foreign_t pl_clingo_ground(term_t ccontrol, term_t parts) {
    clingo_env *ctl;
    clingo_part_t *part_vec = NULL;
    size_t plen = 0;
    int rc;

    if (!(rc = get_clingo(ccontrol, &ctl))) {
        goto out;
    }

    switch (PL_skip_list(parts, 0, &plen)) {
    case PL_LIST: {
        term_t tail = PL_copy_term_ref(parts);
        term_t head = PL_new_term_ref();

        if (!(part_vec = malloc(sizeof(*part_vec) * plen))) {
            rc = PL_resource_error("memory");
            goto out;
        }
        memset(part_vec, 0, sizeof(*part_vec) * plen);

        for (size_t i = 0; PL_get_list(tail, head, tail); i++) {
            if (!(rc = get_params(head, &part_vec[i]))) {
                goto out;
            }
        }
        break;
    }
    default: {
        rc = PL_type_error("list", parts);
        goto out;
    }
    }

    if (!(rc = clingo_status(clingo_control_ground(ctl->control, part_vec, plen,
                                                   call_function, ctl)))) {
        goto out;
    }

out:
    if (part_vec) {
        for (size_t i = 0; i < plen; i++) {
            if (part_vec[i].params) {
                free((void *)part_vec[i].params);
            }
        }
        free(part_vec);
    }

    return rc;
}

static foreign_t pl_clingo_assign_external(term_t ccontrol, term_t Atom,
                                           term_t Value) {
    clingo_env *ctl;
    clingo_symbol_t atom;
    clingo_truth_value_t value;
    int bv, rc;

    if (!(rc = get_clingo(ccontrol, &ctl))) {
        goto out;
    }

    if (!(rc = clingo_status(get_value(Atom, &atom, FALSE)))) {
        goto out;
    }

    if (PL_is_variable(Value)) {
        value = clingo_truth_value_free;
    } else if (PL_get_bool_ex(Value, &bv)) {
        value = bv ? clingo_truth_value_true : clingo_truth_value_false;
    } else {
        rc = PL_domain_error("assign_external", Value);
        goto out;
    }

    if (!(rc = clingo_status(
              clingo_control_assign_external(ctl->control, atom, value)))) {
        goto out;
    }

out:
    return rc;
}

static foreign_t pl_clingo_release_external(term_t ccontrol, term_t Atom) {
    clingo_env *ctl;
    clingo_symbol_t atom;
    int rc;

    if (!(rc = get_clingo(ccontrol, &ctl))) {
        goto out;
    }

    if (!(rc = clingo_status(get_value(Atom, &atom, FALSE)))) {
        goto out;
    }

    if (!(rc = clingo_status(
              clingo_control_release_external(ctl->control, atom)))) {
        goto out;
    }

out:
    return rc;
}

static int unify_value(term_t t, clingo_symbol_t v) {
    // NOTE: the clingo_symbol_* functions below only fail
    //       if applied to the wrong type
    //       they do not allocate
    switch (clingo_symbol_type(v)) {
    case clingo_symbol_type_number: {
        int number;
        clingo_symbol_number(v, &number);
        return PL_unify_integer(t, number);
    }
    case clingo_symbol_type_string: {
        char const *str;
        clingo_symbol_string(v, &str);
        return PL_unify_chars(t, PL_STRING | REP_UTF8, (size_t)-1, str);
    }
    case clingo_symbol_type_infimum: {
        return PL_unify_term(t, PL_FUNCTOR, FUNCTOR_hash1, PL_ATOM, ATOM_inf);
    }
    case clingo_symbol_type_supremum: {
        return PL_unify_term(t, PL_FUNCTOR, FUNCTOR_hash1, PL_ATOM, ATOM_sup);
    }
    case clingo_symbol_type_function: {
        // FIXME: functions can have signs represented as -f(x) in gringo
        char const *str;
        clingo_symbol_t const *args;
        size_t size;
        int rc;

        clingo_symbol_name(v, &str);
        clingo_symbol_arguments(v, &args, &size);

        if (size == 0) {
            if (!(rc =
                      PL_unify_chars(t, PL_ATOM | REP_UTF8, (size_t)-1, str))) {
                goto out_function;
            }
        } else {
            clingo_symbol_t const *it, *ie;
            atom_t name;
            term_t arg;
            int i;

            name = PL_new_atom(str);
            if (!(rc = PL_unify_functor(t, PL_new_functor(name, size)))) {
                goto out_function;
            }
            PL_unregister_atom(name);

            arg = PL_new_term_ref();
            for (i = 1, it = args, ie = it + size; it != ie; ++it, i++) {
                _PL_get_arg(i, t, arg);
                if (!unify_value(arg, *it)) {
                    goto out_function;
                }
            }
        }

    out_function:
        return rc;
    }
    default:
        assert(FALSE);
        return FALSE;
    }
}

static int unify_list_from_span(term_t list, clingo_symbol_t const *syms,
                                size_t slen) {
    int rc;
    term_t tail = PL_copy_term_ref(list);
    term_t head = PL_new_term_ref();
    term_t tmp = PL_new_term_ref();
    clingo_symbol_t const *it, *ie;

    for (it = syms, ie = it + slen; it != ie; ++it) {
        PL_put_variable(tmp);

        if (!(rc = (unify_value(tmp, *it) && PL_unify_list(tail, head, tail) &&
                    PL_unify(head, tmp)))) {
            goto out;
        }
    }

    if (!(rc = PL_unify_nil(tail))) {
        goto out;
    }
out:
    return rc;
}

static int unify_model(term_t t, int show, clingo_model_t *model) {
    clingo_symbol_t *atoms = NULL;
    size_t alen;
    int rc;

    if (!(rc = clingo_status(clingo_model_symbols_size(model, show, &alen)))) {
        goto out;
    }

    atoms = malloc(sizeof(*atoms) * alen);
    if (!atoms) {
        rc = PL_resource_error("memory");
        goto out;
    }

    if (!(rc = clingo_status(clingo_model_symbols(model, show, atoms, alen)))) {
        goto out;
    }

    if (!(rc = unify_list_from_span(t, atoms, alen))) {
        goto out;
    }
out:
    free(atoms);
    return rc;
}

static int get_assumption(term_t t, clingo_symbolic_literal_t *assump) {
    if (PL_is_functor(t, FUNCTOR_tilde1)) {
        _PL_get_arg(1, t, t);
        assump->positive = FALSE;
    } else {
        assump->positive = TRUE;
    }

    return get_value(t, &assump->symbol, FALSE);
}

static int get_show_map(term_t t, int *map) {
    int rc;
    term_t tail = PL_copy_term_ref(t);
    term_t head = PL_new_term_ref();

    *map = 0;
    while (PL_get_list(tail, head, tail)) {
        atom_t a;

        if (!(rc = PL_get_atom_ex(head, &a))) {
            goto out;
        }

        if (a == ATOM_atoms) {
            *map |= clingo_show_type_atoms;
        } else if (a == ATOM_terms) {
            *map |= clingo_show_type_terms;
        } else if (a == ATOM_shown) {
            *map |= clingo_show_type_shown;
        } else if (a == ATOM_csp) {
            *map |= clingo_show_type_csp;
        } else if (a == ATOM_comp) {
            *map |= clingo_show_type_complement;
        } else {
            rc = PL_domain_error("clingo_show", head);
            goto out;
        }
    }

    if (!(rc = PL_get_nil_ex(tail))) {
        goto out;
    }
out:
    return rc;
}

typedef struct solve_state {
    clingo_env *ctl;
    clingo_solve_iteratively_t *it;
} solve_state;

static foreign_t pl_clingo_solve(term_t ccontrol, term_t assumptions,
                                 term_t Show, term_t Model, control_t h) {
    int rc = TRUE;
    solve_state *state = NULL;
    clingo_symbolic_literal_t *assump_vec = NULL;
    int control = PL_foreign_control(h);
    if (control == PL_FIRST_CALL) {
        size_t alen = 0;

        if (!(state = malloc(sizeof(*state)))) {
            rc = PL_resource_error("memory");
            goto out;
        }
        memset(state, 0, sizeof(*state));

        if (!(rc = get_clingo(ccontrol, &state->ctl))) {
            goto out;
        }

        if (PL_skip_list(assumptions, 0, &alen) != PL_LIST) {
            rc = PL_type_error("list", assumptions);
            goto out;
        }

        term_t tail = PL_copy_term_ref(assumptions);
        term_t head = PL_new_term_ref();

        if (!(assump_vec = malloc(sizeof(*assump_vec) * alen))) {
            rc = PL_resource_error("memory");
            goto out;
        }
        memset(assump_vec, 0, sizeof(*assump_vec) * alen);
        for (size_t i = 0; PL_get_list(tail, head, tail); i++) {
            if (!(rc = clingo_status(get_assumption(head, &assump_vec[i])))) {
                goto out;
            }
        }

        if (!(rc = clingo_status(clingo_control_solve_iteratively(
                  state->ctl->control, assump_vec, alen, &state->it)))) {
            goto out;
        }
    } else {
        state = PL_foreign_context_address(h);
    }

    while (control != PL_PRUNED) {
        clingo_model_t *model;

        if (!(rc = clingo_status(
                  clingo_solve_iteratively_next(state->it, &model)))) {
            goto out;
        }
        if (model) {
            int show;

            if (!(rc = get_show_map(Show, &show))) {
                goto out;
            }

            if (!(rc = unify_model(Model, show, model))) {
                if (PL_exception(0)) {
                    goto out;
                }
            } else {
                PL_retry_address(state);
                state = NULL;
                break;
            }

        } else {
            rc = FALSE;
            break;
        }
    }

out:
    if (assump_vec) {
        free(assump_vec);
    }
    if (state) {
        if (state->it) {
            clingo_solve_iteratively_close(state->it);
        }
        free(state);
    }
    return rc;
}

static int unify_symbolic_atom(term_t result, clingo_symbolic_atoms_t *atoms, clingo_symbolic_atom_iterator_t it) {
    clingo_symbol_t val;
    if (!clingo_status(clingo_symbolic_atoms_symbol(atoms, it, &val))) {
        return FALSE;
    }
    bool fact;
    if (!clingo_status(clingo_symbolic_atoms_is_fact(atoms, it, &fact))) {
        return FALSE;
    }
    bool external;
    if (!clingo_status(clingo_symbolic_atoms_is_external(atoms, it, &external))) {
        return FALSE;
    }
    term_t symbol = PL_new_term_ref();
    return unify_value(symbol, val) &&
            PL_unify_term(result,
                          PL_FUNCTOR, FUNCTOR_symbol2,
                          PL_TERM, symbol,
                          PL_ATOM, fact ? ATOM_fact : (external ? ATOM_external : ATOM_unknown));
}

static foreign_t pl_symbol_lookup(term_t control, term_t symbol, term_t result) {
    clingo_env *ccontrol;
    clingo_symbolic_atoms_t *atoms;
    clingo_symbol_t val;
    clingo_symbolic_atom_iterator_t it;
    bool valid;

    return
        get_clingo(control, &ccontrol) &&
        clingo_status(clingo_control_symbolic_atoms(ccontrol->control, &atoms)) &&
        get_value(symbol, &val, FALSE) &&
        clingo_status(clingo_symbolic_atoms_find(atoms, val, &it)) &&
        clingo_status(clingo_symbolic_atoms_is_valid(atoms, it, &valid)) &&
        valid && unify_symbolic_atom(result, atoms, it);
}

static foreign_t pl_symbol_list_next(term_t context, term_t slice, term_t list, term_t diff_tail) {
    clingo_symbolic_atoms_t *atoms;
    clingo_symbolic_atom_iterator_t it;
    term_t tmp = PL_new_term_ref();
    int i, n;
    if (!(PL_get_arg(1, context, tmp) &&
            PL_get_pointer_ex(tmp, (void**)&atoms) &&
            PL_get_arg(2, context, tmp) &&
            PL_get_int64_ex(tmp, (int64_t*)&it) &&
            PL_get_integer_ex(slice, &n))) { return FALSE; }
    term_t tail = PL_copy_term_ref(list);
    term_t head = PL_new_term_ref();
    for (i = 0; i != n; ++i) {
        bool valid;
        if (!clingo_status(clingo_symbolic_atoms_is_valid(atoms, it, &valid))) { return FALSE; }
        if (valid) {
            if (!(PL_unify_list(tail, head, tail) && unify_symbolic_atom(head, atoms, it))) { return FALSE; }
        }
        else { break; }
    }
    return PL_unify(tail, diff_tail);
}

static foreign_t pl_symbol_list(term_t control, term_t context) {
    clingo_env *ccontrol;
    clingo_symbolic_atoms_t *atoms;
    clingo_symbolic_atom_iterator_t it;
    return
        get_clingo(control, &ccontrol) &&
        clingo_status(clingo_control_symbolic_atoms(ccontrol->control, &atoms)) &&
        clingo_status(clingo_symbolic_atoms_begin(atoms, NULL, &it)) &&
        PL_unify_term(context, PL_FUNCTOR, FUNCTOR_symbolic_atoms2, PL_POINTER, atoms, PL_INT64, (int64_t)it);
}
////////////////////////////// CALLBACK //////////////////////////////

static bool get_value(term_t t, clingo_symbol_t *val, int minus) {
    switch (PL_term_type(t)) {
    case PL_INTEGER: {
        int i;

        if (PL_get_integer(t, &i)) {
            clingo_symbol_create_number(i, val);
            return true;
        }
        return false;
    }
    case PL_ATOM: {
        char *s;
        size_t len;

        if (PL_get_nchars(t, &len, &s, CVT_ATOM | REP_UTF8 | CVT_EXCEPTION)) {
            return clingo_symbol_create_id(s, !minus, val); /* no sign */
        }
        return false;
    }
    case PL_STRING: {
        char *s;
        size_t len;

        if (PL_get_nchars(t, &len, &s, CVT_STRING | REP_UTF8 | CVT_EXCEPTION)) {
            return clingo_symbol_create_string(s, val);
        }
        return false;
    }
    case PL_TERM: {
        bool rc;
        term_t arg;
        atom_t name;
        size_t arity; /* TBD: -atom, #const */
        clingo_symbol_t *values = NULL;

        if (!(rc = get_name_arity(t, &name, &arity))) {
            clingo_set_error(clingo_error_runtime, "prolog error");
            goto out_term;
        }
        arg = PL_new_term_ref();

        if (name == ATOM_minus && arity == 1) {
            if (!(rc = get_value(arg, val, TRUE))) {
                goto out_term;
            }
        } else if (name == ATOM_hash && arity == 1) {
            atom_t a;

            _PL_get_arg(1, t, arg);
            if (!(rc = PL_get_atom_ex(arg, &a))) {
                clingo_set_error(clingo_error_runtime, "prolog error");
                goto out_term;
            }

            if (a == ATOM_inf) {
                clingo_symbol_create_infimum(val);
            } else if (a == ATOM_sup) {
                clingo_symbol_create_supremum(val);
            } else {
                rc = false;
                clingo_set_error(clingo_error_runtime, "bad value");
                goto out_term;
            }
        } else {
            const char *id = PL_atom_chars(name); /* TBD: errors */
            size_t i;

            if (!(values = malloc(sizeof(*values) * arity))) {
                rc = false;
                clingo_set_error(clingo_error_bad_alloc, "memory");
                goto out_term;
            }

            for (i = 0; i < arity; i++) {
                _PL_get_arg(i + 1, t, arg);
                if (!(rc = get_value(arg, &values[i], FALSE))) {
                    goto out_term;
                }
            }
            PL_reset_term_refs(arg);

            if (!(rc = clingo_symbol_create_function(id, values, arity, !minus,
                                                     val))) {
                goto out_term;
            }
        }
    out_term:
        if (values) {
            free(values);
        }
        return rc;
    }
    default:
        clingo_set_error(clingo_error_runtime, "bad value");
        return false;
    }
}

static bool call_function(clingo_location_t const *loc, char const *name,
                          clingo_symbol_t const *in, size_t ilen, void *closure,
                          clingo_symbol_callback_t cb, void *cb_closure) {
    (void)loc;
    (void)closure;
    static predicate_t pred = 0;
    fid_t fid = 0;
    qid_t qid = 0;
    term_t av;
    bool rc = true;

    if (!pred) {
        pred = PL_predicate("inject_values", 3, "clingo");
    }

    if (!(fid = PL_open_foreign_frame())) {
        rc = false;
        clingo_set_error(clingo_error_runtime, "prolog error");
        goto out;
    }

    av = PL_new_term_refs(3);

    PL_put_atom_chars(av + 0, name);
    if (!(rc = unify_list_from_span(av + 1, in, ilen))) {
        clingo_set_error(clingo_error_runtime, "prolog error");
        goto out;
    }
    if ((qid = PL_open_query(NULL, PL_Q_PASS_EXCEPTION, pred, av))) {
        while (PL_next_solution(qid)) {
            clingo_symbol_t value;
            if (!(rc = get_value(av + 2, &value, FALSE))) {
                goto out;
            }
            if (!(rc = cb(&value, 1, cb_closure))) {
                goto out;
            }
        }
        if (PL_exception(0)) {
            rc = false;
            clingo_set_error(clingo_error_runtime, "prolog error");
            goto out;
        }
    }

out:
    if (qid) {
        PL_close_query(qid);
    }
    if (fid) {
        PL_close_foreign_frame(fid);
    }

    return rc;
}

install_t install_clingo(void) {
    ATOM_sup = PL_new_atom("sup");
    ATOM_inf = PL_new_atom("inf");
    ATOM_minus = PL_new_atom("-");
    ATOM_hash = PL_new_atom("#");
    ATOM_atoms = PL_new_atom("atoms");
    ATOM_terms = PL_new_atom("terms");
    ATOM_shown = PL_new_atom("shown");
    ATOM_csp = PL_new_atom("csp");
    ATOM_comp = PL_new_atom("comp");
    ATOM_fact = PL_new_atom("fact");
    ATOM_external = PL_new_atom("external");
    ATOM_unknown = PL_new_atom("unknown");
    FUNCTOR_hash1 = PL_new_functor(ATOM_hash, 1);
    FUNCTOR_tilde1 = PL_new_functor(PL_new_atom("~"), 1);
    FUNCTOR_clingo_error1 = PL_new_functor(PL_new_atom("clingo_error"), 1);
    FUNCTOR_symbol2 = PL_new_functor(PL_new_atom("symbol"), 2);
    FUNCTOR_symbolic_atoms2 = PL_new_functor(PL_new_atom("$symbolic_atoms"), 2);
    FUNCTOR_error2 = PL_new_functor(PL_new_atom("error"), 2);

    PL_register_foreign("clingo_new", 2, pl_clingo_new, 0);
    PL_register_foreign("clingo_close", 1, pl_clingo_close, 0);
    PL_register_foreign("clingo_add", 3, pl_clingo_add, 0);
    PL_register_foreign("clingo_ground", 2, pl_clingo_ground, 0);
    PL_register_foreign("clingo_solve", 4, pl_clingo_solve,
                        PL_FA_NONDETERMINISTIC);
    PL_register_foreign("clingo_assign_external", 3, pl_clingo_assign_external,
                        0);
    PL_register_foreign("clingo_release_external", 2,
                        pl_clingo_release_external, 0);
    PL_register_foreign("clingo_symbol_lookup", 3,
                        pl_symbol_lookup, 0);
    PL_register_foreign("clingo_symbol_list", 2,
                        pl_symbol_list, 0);
    PL_register_foreign("clingo_symbol_list_next", 4,
                        pl_symbol_list_next, 0);
}
