/*
 * nn_graph.h - The pointers each object of the C data layer holds.
 *
 * A walk over the value graph (a census's mark, a region's) needs to know,
 * for every kind of object, which other objects it points at.  That
 * knowledge lives here alone: nn_graph_children hands each pointer an
 * object holds to a visitor, which decides what reaching it means.
 *
 * Objects are thunk cells, envs, attribute sets, list headers, lambdas,
 * builtins' partial applications and strings with context.  Three arrays
 * live in the env pages and are reached through an object, not on their
 * own: an env's with-scopes, a list's items, and a deferred application's
 * cells.  A visitor is handed each array with the object that holds it,
 * and follows the entries it chooses with nn_graph_entry, so a walk can
 * tell an array several owners share from one it has not seen.  Strings
 * and symbols hold no pointer and are handed over as leaves.
 *
 * A NULL pointer is never handed over: an empty slot, an absent parent, a
 * context element without an output.  Statics (the null, true, false and
 * empty list cells, the builtins' cells, the empty env, set and string)
 * are, and the visitor tells them apart.
 */

#ifndef NN_GRAPH_H
#define NN_GRAPH_H

#include <stdint.h>

#include "nn_str.h"

/* --- Kinds --- */

/* Objects. */
#define NN_GRAPH_THUNK      0
#define NN_GRAPH_ENV        1
#define NN_GRAPH_ATTRSET    2
#define NN_GRAPH_LIST       3
#define NN_GRAPH_LAMBDA     4
#define NN_GRAPH_PRIMOP_APP 5
#define NN_GRAPH_CTXSTR     6

/* Arrays of the env pages, each reached through the object that holds
 * it: entries tagged NN_ENV_LAZY_WITH or sets; cells; cells. */
#define NN_GRAPH_WITH_SCOPES 0
#define NN_GRAPH_ITEMS       1
#define NN_GRAPH_APP_CELLS   2

/* --- Visitor --- */

typedef struct nn_graph_visitor {
    /* An object of an NN_GRAPH_* object kind. */
    void (*object)(void *ctx, int kind, const void *object);
    /* An array of an NN_GRAPH_* array kind: `count` entries from `base`,
     * held by `owner` (the env, list header or thunk). */
    void (*array)(void *ctx, int kind, const void *owner, void *const *base, uint32_t count);
    void (*string)(void *ctx, const nn_str_t *s);
    void (*symbol)(void *ctx, uint32_t sym);
    /* A thunk state, pending form or value tag no object has. */
    void (*unknown)(void *ctx);
} nn_graph_visitor_t;

/* Hand every pointer the object holds to the visitor.  An object kind
 * nn_graph does not name goes to `unknown`. */
void nn_graph_children(int kind, const void *object, const nn_graph_visitor_t *visitor, void *ctx);

/* Hand the object an array entry holds to the visitor: a set or a lazy
 * with-scope's thunk for NN_GRAPH_WITH_SCOPES, a cell otherwise.  A NULL
 * entry hands over nothing. */
void nn_graph_entry(int kind, const void *entry, const nn_graph_visitor_t *visitor, void *ctx);

#endif /* NN_GRAPH_H */
