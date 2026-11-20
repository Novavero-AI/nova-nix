/*
 * nn_region.h - A census of regions: what the C data layer could free at
 * the end of each force of a thunk, measured and never freed.
 *
 * A region is the extent of one force: everything the data layer
 * allocates from the force's start to the store of its value, inner
 * forces included.  Once the force returns, an object it allocated is
 * reachable only through an object older than the force that the force
 * wrote a pointer into (the forced cell, a cell an inner force memoized,
 * a with-scope entry resolved to a set) or through a root registered
 * during it (nn_roots.h).  The census records every such write: each
 * store into a cell (nn_thunk.h) and each resolved with-scope entry
 * (nn_env_resolve_with).  At a region's close it marks from the records
 * of objects older than the region and from the roots registered since
 * it opened, and stops at every object older than the region.  What the
 * mark does not reach is what a collector would free at that close.  The
 * census frees nothing and writes nothing in the data layer.
 *
 * Age.  Each block objects are bumped from (a thunk block, an env page, a
 * block of the value string arena) is numbered as its allocator makes it,
 * and an address index finds the block of any pointer into one.  An
 * object is younger than a region when its block was made after the
 * region opened, or when it lies in the block its allocator bumped from
 * as the region opened, at or past what that block had handed out then.
 * An object malloc'd on its own and tracked (nn_tracker.h) is younger
 * when its place in its tracker is at or past the tracker's count as the
 * region opened.
 *
 * A close passes to the region around it the records of objects older
 * than that region as well, and drops the rest: a write into an object
 * the enclosing region made needs no record there.
 *
 * Figures.  Each close counts the bytes its region allocated, the bytes
 * the mark reached in it, the objects the mark entered and the pointers
 * it examined, by size class: the bit length of the bytes allocated.  The
 * census also simulates a collector that marks only the regions that
 * allocated at least a threshold of bytes, a smaller one passing what it
 * allocated to the region around it unexamined.  For each threshold it
 * gives the closes that mark, their work, and the most bytes held at
 * once, by grain: freeing every object nothing reaches, or freeing only
 * whole blocks that hold nothing reached, at the allocators' block sizes
 * and at each of four sizes a quarter of the one before.
 *
 * A check of the premise.  A unit of a block or a tracked object a close
 * finds unreached is marked as having died, and a later close that
 * reaches it counts it as resurrected: a pointer the records and roots
 * did not hold reached it, and a collector freeing at the first close
 * would have left that pointer dangling.
 *
 * A census runs from before the arena's init to after its teardown
 * (nn_region_census_begin and nn_region_census_end), so that it numbers
 * every block.  When one of its tables cannot grow it stops counting and
 * says so (NN_REGION_TOTAL_FAILED).  Not thread-safe.
 */

#ifndef NN_REGION_H
#define NN_REGION_H

#include <stdint.h>

struct nn_thunk;

/* --- Kinds the allocators name --- */

/* Blocks objects are bumped from. */
#define NN_REGION_THUNK_BLOCK 0
#define NN_REGION_ENV_PAGE    1
#define NN_REGION_STR_BLOCK   2
#define NN_REGION_BUMP_KINDS  3

/* Blocks of the compiled code's strings, outside every region as code is
 * (nn_bytecode.h): indexed so that a string the census meets is placed
 * in a block of values, in one of code, or counted as unplaced. */
#define NN_REGION_CODE_BLOCK  3

/* Objects malloc'd one at a time and tracked. */
#define NN_REGION_SET           0
#define NN_REGION_LIST          1
#define NN_REGION_LAMBDA        2
#define NN_REGION_PRIMOP_APP    3
#define NN_REGION_CTXSTR        4
#define NN_REGION_TRACKED_KINDS 5

/* Nonzero while a census runs.  The allocators' hooks below are called
 * only then. */
extern int nn_region_census;

/* --- Lifecycle --- */

/* Start a census, while no arena is live (NN_ASSERT).  Returns 0, or -1
 * with no census running when its tables cannot be allocated. */
int nn_region_census_begin(void);

/* End the census and release its tables, after the arena's teardown. */
void nn_region_census_end(void);

/* --- The allocators' half --- */

/* A block of `capacity` bytes from `data`, made for objects of a bump
 * kind or for the code's strings, and whether its allocator now bumps
 * from it.  Every block is at least the address index's unit
 * (NN_ASSERT). */
void nn_region_block(int kind, const char *data, uint64_t capacity, int current);

/* A tracked object of a tracked kind, at `position` in its tracker,
 * holding `bytes`; and bytes a tracked object took on as it grew. */
void nn_region_tracked(int kind, const void *object, uint32_t position, uint64_t bytes);
void nn_region_tracked_grew(uint64_t bytes);

/* A list header at `position` in its tracker: a new list's, or one that
 * `of` dropped elements of (nn_list_drop), whose items are the tail of
 * the array `of` spans.  An allocator frees an items array whole, so the
 * census keeps the whole array while any list over it is reached, and
 * follows only the elements each list spans. */
void nn_region_list(const void *list, uint32_t position, const void *of);

/* A pointer stored into a cell, and into a with-scope entry. */
void nn_region_record_cell(const struct nn_thunk *cell);
void nn_region_record_with(void *const *entry);

/* --- The evaluator's half --- */

/* Open a region as a force starts, and close the innermost as it ends,
 * after its value is stored or as it throws.  Nothing while no census
 * runs. */
void nn_region_open(void);
void nn_region_close(void);

/* --- Figures --- */

/* The census's totals, NN_REGION_TOTALS of them. */
#define NN_REGION_TOTAL_CLOSES            0  /* regions closed */
#define NN_REGION_TOTAL_ALLOCATED         1  /* bytes allocated since the census began */
#define NN_REGION_TOTAL_OUTSIDE           2  /* of those, bytes no region has allocated */
#define NN_REGION_TOTAL_DEPTH             3  /* the most regions open at once */
#define NN_REGION_TOTAL_RECORDS           4  /* the most records held at once */
#define NN_REGION_TOTAL_UNPLACED          5  /* pointers the census could not place */
#define NN_REGION_TOTAL_RESURRECTED       6  /* objects and array entries reached after a close found them unreached */
#define NN_REGION_TOTAL_RESURRECTED_BYTES 7
#define NN_REGION_TOTAL_FAILED            8  /* nonzero when a table could not grow */
#define NN_REGION_TOTALS                  9

/* Closes by size class: class 0 allocated nothing, class n from 2^(n-1)
 * bytes to under 2^n, and the last class everything from its floor up.
 * For each, NN_REGION_CLASS_FIGURES figures. */
#define NN_REGION_SIZE_CLASSES 48
#define NN_REGION_CLASS_CLOSES    0
#define NN_REGION_CLASS_ALLOCATED 1  /* bytes */
#define NN_REGION_CLASS_SURVIVING 2  /* bytes reached at the close */
#define NN_REGION_CLASS_ENTERED   3  /* objects the mark entered */
#define NN_REGION_CLASS_EXAMINED  4  /* pointers the mark examined */
#define NN_REGION_CLASS_RECORDS   5  /* records the close read */
#define NN_REGION_CLASS_FIGURES   6

/* Thresholds: 0, then NN_REGION_THRESHOLD_FIRST bytes, each after it
 * four times the one before. */
#define NN_REGION_THRESHOLDS      9
#define NN_REGION_THRESHOLD_FIRST 4096u

/* Grains: 0 frees single objects; grain g from 1 frees whole blocks of
 * the allocator's block size divided by 4^(g-1). */
#define NN_REGION_GRAINS 6

/* For each threshold, NN_REGION_THRESHOLD_FIGURES figures: the closes
 * that mark, the objects they entered, the pointers they examined, and
 * then the most bytes held at once at each grain. */
#define NN_REGION_THRESHOLD_CLOSES   0
#define NN_REGION_THRESHOLD_ENTERED  1
#define NN_REGION_THRESHOLD_EXAMINED 2
#define NN_REGION_THRESHOLD_PEAK     3
#define NN_REGION_THRESHOLD_FIGURES  (NN_REGION_THRESHOLD_PEAK + NN_REGION_GRAINS)

/* What is allocated outside every region is what the evaluator does
 * between forces, at the top of an evaluation or in a function it calls
 * there, which no close can free; read with no region open, it counts
 * none still open. */
void nn_region_totals(uint64_t *totals);
void nn_region_classes(uint64_t *figures);     /* NN_REGION_SIZE_CLASSES rows */
void nn_region_thresholds(uint64_t *figures);  /* NN_REGION_THRESHOLDS rows */

/* The bytes of threshold k, and of a block of a bump kind at grain g from
 * 1. */
uint64_t nn_region_threshold_bytes(uint32_t k);
uint64_t nn_region_grain_bytes(int kind, uint32_t grain);

#endif /* NN_REGION_H */
