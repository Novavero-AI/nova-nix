/*
 * nn_graph.c - The pointers each object of the C data layer holds
 * (nn_graph.h).
 */

#include "nn_graph.h"

#include "nn_ctxstr.h"
#include "nn_env.h"
#include "nn_lambda.h"
#include "nn_list.h"
#include "nn_primop.h"
#include "nn_attrset.h"
#include "nn_thunk.h"

static void object(const nn_graph_visitor_t *v, void *ctx, int kind, const void *p)
{
    if (p) v->object(ctx, kind, p);
}

static void string(const nn_graph_visitor_t *v, void *ctx, const nn_str_t *s)
{
    if (s) v->string(ctx, s);
}

static void array(const nn_graph_visitor_t *v, void *ctx, int kind, const void *owner,
                  void *const *base, uint32_t count)
{
    if (base && count > 0) v->array(ctx, kind, owner, base, count);
}

/* A pending or blackholed thunk holds what it will run: a blackhole keeps
 * its pending form and payload (nn_thunk_mark_pending puts it back to
 * work).  A computed one holds its value's object, if the value is one. */
static void thunk_children(const nn_thunk_t *t, const nn_graph_visitor_t *v, void *ctx)
{
    uint8_t state = nn_thunk_state(t);
    if (state == NN_THUNK_PENDING || state == NN_THUNK_BLACKHOLE) {
        uint8_t form = nn_thunk_pending_form(t);
        switch (form) {
        case NN_PENDING_CODE:
            object(v, ctx, NN_GRAPH_ENV, nn_thunk_payload(t));
            return;
        case NN_PENDING_APPLY1:
        case NN_PENDING_APPLY2:
            /* One more cell than the form's arguments: the function's. */
            array(v, ctx, NN_GRAPH_APP_CELLS, t, (void *const *)nn_thunk_payload(t), (uint32_t)form + 1);
            return;
        default:
            v->unknown(ctx);
            return;
        }
    }
    if (state != NN_THUNK_COMPUTED) {
        v->unknown(ctx);
        return;
    }
    switch (nn_thunk_value_tag(t)) {
    case NN_VALUE_INT:
    case NN_VALUE_FLOAT:
    case NN_VALUE_BOOL:
    case NN_VALUE_NULL:
    case NN_VALUE_PRIMOP:
        return;
    case NN_VALUE_STR:
    case NN_VALUE_PATH:
        string(v, ctx, (const nn_str_t *)nn_thunk_payload(t));
        return;
    case NN_VALUE_LIST:
        object(v, ctx, NN_GRAPH_LIST, nn_thunk_payload(t));
        return;
    case NN_VALUE_ATTRS:
        object(v, ctx, NN_GRAPH_ATTRSET, nn_thunk_payload(t));
        return;
    case NN_VALUE_CTXSTR:
        object(v, ctx, NN_GRAPH_CTXSTR, nn_thunk_payload(t));
        return;
    case NN_VALUE_LAMBDA:
        object(v, ctx, NN_GRAPH_LAMBDA, nn_thunk_payload(t));
        return;
    case NN_VALUE_PRIMOP_APP:
        object(v, ctx, NN_GRAPH_PRIMOP_APP, nn_thunk_payload(t));
        return;
    default:
        v->unknown(ctx);
        return;
    }
}

/* An env's slots and its parent, and its name table and with-scopes when
 * it has scopes. */
static void env_children(const nn_env_t *env, const nn_graph_visitor_t *v, void *ctx)
{
    uint32_t i;
    const nn_env_scopes_t *scopes = nn_env_scopes(env);
    uint32_t slot_count = nn_env_slot_count(env);
    for (i = 0; i < slot_count; i++) object(v, ctx, NN_GRAPH_THUNK, env->slots[i]);
    object(v, ctx, NN_GRAPH_ENV, nn_env_parent(env));
    if (!scopes) return;
    object(v, ctx, NN_GRAPH_ATTRSET, scopes->name_table);
    array(v, ctx, NN_GRAPH_WITH_SCOPES, env, (void *const *)scopes->with_scopes, scopes->with_count);
}

static void attrset_children(const nn_attrset_t *set, const nn_graph_visitor_t *v, void *ctx)
{
    uint32_t n = nn_attrset_layer_count(set), i;
    const nn_symbol_t *keys = nn_attrset_layer_keys(set);
    void *const *values = nn_attrset_layer_values(set);
    for (i = 0; i < n; i++) {
        v->symbol(ctx, keys[i]);
        object(v, ctx, NN_GRAPH_THUNK, values[i]);
    }
}

static void lambda_children(const nn_lambda_t *lam, const nn_graph_visitor_t *v, void *ctx)
{
    uint32_t i;
    uint32_t formals = nn_lambda_formal_count(lam);
    object(v, ctx, NN_GRAPH_ENV, lam->env);
    v->symbol(ctx, lam->name_sym);
    v->symbol(ctx, lam->lambda_name_sym);
    for (i = 0; i < formals; i++) v->symbol(ctx, lam->entries[i].name_sym);
}

static void primop_app_children(const nn_primop_app_t *app, const nn_graph_visitor_t *v, void *ctx)
{
    uint16_t i;
    for (i = 0; i < app->taken; i++) object(v, ctx, NN_GRAPH_THUNK, app->args[i]);
}

static void ctxstr_children(const nn_ctxstr_t *s, const nn_graph_visitor_t *v, void *ctx)
{
    uint32_t i;
    string(v, ctx, s->text);
    for (i = 0; i < s->ctx_count; i++) {
        string(v, ctx, s->ctx[i].sp_hash);
        string(v, ctx, s->ctx[i].sp_name);
        string(v, ctx, s->ctx[i].output);
    }
}

void nn_graph_children(int kind, const void *p, const nn_graph_visitor_t *v, void *ctx)
{
    switch (kind) {
    case NN_GRAPH_THUNK:
        thunk_children((const nn_thunk_t *)p, v, ctx);
        return;
    case NN_GRAPH_ENV:
        env_children((const nn_env_t *)p, v, ctx);
        return;
    case NN_GRAPH_ATTRSET:
        attrset_children((const nn_attrset_t *)p, v, ctx);
        return;
    case NN_GRAPH_LIST: {
        const nn_list_t *list = (const nn_list_t *)p;
        array(v, ctx, NN_GRAPH_ITEMS, list, (void *const *)list->items, list->count);
        return;
    }
    case NN_GRAPH_LAMBDA:
        lambda_children((const nn_lambda_t *)p, v, ctx);
        return;
    case NN_GRAPH_PRIMOP_APP:
        primop_app_children((const nn_primop_app_t *)p, v, ctx);
        return;
    case NN_GRAPH_CTXSTR:
        ctxstr_children((const nn_ctxstr_t *)p, v, ctx);
        return;
    default:
        v->unknown(ctx);
        return;
    }
}

void nn_graph_entry(int kind, const void *entry, const nn_graph_visitor_t *v, void *ctx)
{
    if (kind == NN_GRAPH_WITH_SCOPES) {
        uintptr_t word = (uintptr_t)entry;
        if (word & NN_ENV_LAZY_WITH)
            object(v, ctx, NN_GRAPH_THUNK, (const void *)(word & ~NN_ENV_LAZY_WITH));
        else
            object(v, ctx, NN_GRAPH_ATTRSET, entry);
        return;
    }
    object(v, ctx, NN_GRAPH_THUNK, entry);
}
