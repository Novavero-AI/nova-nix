/*
 * nn_arena.c - Whether every sub-arena of the C data layer is live.
 */

#include "nn_arena.h"
#include "nn_bytecode.h"
#include "nn_env.h"
#include "nn_str.h"
#include "nn_symbol.h"
#include "nn_thunk.h"

int
nn_arena_live(void)
{
    return nn_symbol_live() && nn_str_live() && nn_thunk_live()
        && nn_env_live() && nn_bytecode_live();
}
