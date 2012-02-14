#include <stdio.h>
#include <string.h>

#include "erl_nif.h"

#include "atomic_ops.h"
#include "stm.h"
#include "wrappers.h"

static ErlNifResourceType* stm_erl_RESOURCE;

enum stm_erl_var_field_type {
    INT,
    BIN
};

typedef struct {
    enum stm_erl_var_field_type type;
    void *field;
    unsigned int size;
} stm_erl_var;

typedef struct {
    stm_erl_var *var;
} stm_erl_handle;

ERL_NIF_TERM stm_erl_init(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    stm_init();
    return enif_make_atom(env, "ok");
}

ERL_NIF_TERM stm_erl_close(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    stm_exit();
    return enif_make_atom(env, "ok");
}

ERL_NIF_TERM stm_erl_trans_start(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    stm_init_thread();
    stm_tx_attr_t *_a = enif_alloc(sizeof(*_a));
    (*_a).id = rand();
    (*_a).no_retry = 1;
    stm_start(_a);
    return enif_make_atom(env, "ok");
}

ERL_NIF_TERM stm_erl_commit(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    int aborted = stm_aborted();
    aborted ? stm_abort(STM_ABORT_EXPLICIT) : stm_commit();
    stm_exit_thread();
    return enif_make_atom(env, aborted == 0 ? "ok" : "error");
}

ERL_NIF_TERM stm_erl_new_var(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    stm_erl_handle* handle = (stm_erl_handle*)enif_alloc_resource(stm_erl_RESOURCE,
                                                         sizeof(stm_erl_handle));

    handle->var = enif_alloc(sizeof(stm_erl_var));

    long value;
    if (enif_get_int64(env, argv[0], &value)) {

        handle->var->field = enif_alloc(sizeof(int64_t));
        handle->var->type = INT;
        handle->var->size = sizeof(int);

        stm_store_long(handle->var->field, value);
    }
    else
        return enif_make_badarg(env);

    ERL_NIF_TERM result = enif_make_resource(env, handle);
    enif_release_resource(handle);

    return enif_make_tuple2(env, enif_make_atom(env, "ok"), result);
}

ERL_NIF_TERM stm_erl_load_var(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    stm_erl_handle* handle;
    if (!enif_get_resource(env, argv[0], stm_erl_RESOURCE, (void**)&handle))
        return enif_make_badarg(env);
    long value = stm_load_long(handle->var->field);
    return enif_make_int64(env, value);
}

ERL_NIF_TERM stm_erl_store_var(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    stm_erl_handle* handle;
    long value;
    if (!enif_get_resource(env, argv[1], stm_erl_RESOURCE, (void**)&handle) ||
        !enif_get_int64(env, argv[0], &value))
        return enif_make_badarg(env);

    stm_store_long(handle->var->field, value);

    return enif_make_atom(env, "ok");
}

static void stm_erl_resource_resource_cleanup(ErlNifEnv* env, void* arg) {

}

static int on_load(ErlNifEnv* env, void** priv_data, ERL_NIF_TERM load_info)
{
    ErlNifResourceFlags flags = (ErlNifResourceFlags)(ERL_NIF_RT_CREATE | ERL_NIF_RT_TAKEOVER);
    stm_erl_RESOURCE = enif_open_resource_type(env,
                                            "stm",
                                            "stm_erl_resource",
                                            &stm_erl_resource_resource_cleanup,
                                            flags,
                                            0);

    return 0;
}

static ErlNifFunc nif_funcs[] =
{
    {"initialize", 0, stm_erl_init},
    {"close", 0, stm_erl_close},
    {"trans_start", 0, stm_erl_trans_start},
    {"commit", 0, stm_erl_commit},
    {"new_var", 1, stm_erl_new_var},
    {"store_var", 2, stm_erl_store_var},
    {"load_var", 1, stm_erl_load_var}
};

ERL_NIF_INIT(stm, nif_funcs, &on_load, NULL, NULL, NULL);
