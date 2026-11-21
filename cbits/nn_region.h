/*
 * nn_region.h - Regions: the C data layer's collector, which frees at the
 * end of each force of a thunk what the force allocated that nothing
 * reaches, and a census that measures what it and other collectors would
 * free there, freeing nothing.
 *
 * A region is the extent of one force: everything the data layer
 * allocates from the force's start to the store of its value, inner
 * forces included.  Once the force returns, an object it allocated is
 * reachable only through an object older than the force that the force
 * wrote a pointer into (the forced cell, a cell an inner force memoized,
 * a with-scope entry resolved to a set) or through a root registered
 * during it (nn_roots.h).  Regions record every such write: each store
 * into a cell (nn_thunk.h) and each resolved with-scope entry
 * (nn_env_resolve_with).  At a region's close the census marks from the
 * records of objects older than the region and from the roots registered
 * since it opened, and stops at every object older than the region.  What
 * the mark does not reach is what a collector would free at that close.
 * The census frees nothing and writes nothing in the data layer.
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
 * once, by grain: freeing every object nothing reaches; freeing only
 * whole blocks that hold nothing reached, at the allocators' block sizes
 * and at each of four sizes a quarter of the one before; or freeing only
 * the lines of a block that hold nothing reached, as a collector reusing
 * free lines in place would.
 *
 * A check of the premise.  A unit of a block or a tracked object a close
 * finds unreached is marked as having died, and a later close that
 * reaches it counts it as resurrected: a pointer the records and roots
 * did not hold reached it, and a collector freeing at the first close
 * would have left that pointer dangling.
 *
 * Tenure.  Every mark above enters again, at each close around a region,
 * what the region's close found reached.  The census also simulates
 * collectors that do not: at the closes of regions that allocated at
 * least the tenure threshold, each marks only what no such close of its
 * inside the region tenured, and then tenures the whole region.  A
 * tenured object counts as reached until a close marks its region whole:
 * the outermost close around it, past the threshold, which takes the
 * census's own mark, or for a collector with a budget, any close whose
 * region holds at least that many tenured bytes which no mark of the
 * whole has reached since.  Besides the roots, two kinds of pointer reach an
 * untenured object from outside what a close marks, and the census holds
 * both: a store into a cell or with-scope entry that is tenured or older
 * than the region, kept as a record that the first such close reads; and
 * a pointer that an object a close's mark enters, or a cell a record
 * names, holds into a region around that it is younger than and
 * untenured in, which the close remembers for that region, since no later
 * close but a mark of the whole looks inside the object or reads the
 * record again.  A mark of the whole region reads every record of a cell
 * or entry older than it, which the census keeps for as long as one is,
 * and no remembered pointer: it looks inside everything in the region
 * that holds one.  It counts these
 * collectors' work and the most bytes they hold as for the others, and
 * the bytes their marks of the whole free, and checks that they free
 * nothing the census's own mark at the same close reached, and nothing a
 * later close reaches.
 *
 * The collector.  Collecting, regions run one of the tenure collectors
 * above and nothing else: the one with a budget of 1 MiB, whose close of
 * an outermost region is a mark of the whole by its own mark.  At each
 * close where it marks, it frees the tracked objects of the region its
 * mark left unreached, and the region's part of each tracker closes up
 * behind the places before it (nn_tracker_sweep), so that a tracker's
 * order stays age order and every open region's count stays its start.
 * Its marks are the census's premise at work, so one that reaches a unit
 * it freed, or a tracked object it holds no place for, aborts
 * (NN_ASSERT).  A freed tracked object whose address the allocator has
 * given to another of its kind passes for that one: the sanitizers'
 * quarantine, which holds a freed address back, is what catches a read of
 * it.
 *
 * Regions run from before the arena's init to after its teardown
 * (nn_region_begin and nn_region_end), so that they number every block
 * and place every tracked object.  When one of their tables cannot grow
 * they stop: a census stops counting and says so
 * (NN_REGION_TOTAL_FAILED), and the collector stops freeing, leaving
 * everything after to the arena's teardown.  Not thread-safe.
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

/* What regions run for: nothing, a census, or the collector. */
#define NN_REGION_OFF     0
#define NN_REGION_CENSUS  1
#define NN_REGION_COLLECT 2

/* The mode regions run in, NN_REGION_OFF while none run.  The
 * allocators' hooks below are called only while they run. */
extern int nn_region_mode;

/* --- Lifecycle --- */

/* Start regions in NN_REGION_CENSUS or NN_REGION_COLLECT, while no arena
 * is live (NN_ASSERT).  Returns 0, or -1 with none running when their
 * tables cannot be allocated. */
int nn_region_begin(int mode);

/* Stop them and release their tables, after the arena's teardown. */
void nn_region_end(void);

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
 * after its value is stored or as it throws.  Nothing while no regions
 * run. */
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

/* Grains: 0 frees single objects; grain g from 1 to
 * NN_REGION_BLOCK_GRAINS frees whole blocks of the allocator's block size
 * divided by 4^(g-1); each of the NN_REGION_LINE_GRAINS after them frees
 * lines, the first of NN_REGION_LINE_FIRST bytes and each after it half
 * the one before.  A line is a block's bytes from a multiple of its size
 * in. */
#define NN_REGION_BLOCK_GRAINS 5
#define NN_REGION_LINE_GRAINS  2
#define NN_REGION_LINE_FIRST   256u
#define NN_REGION_GRAINS       (1 + NN_REGION_BLOCK_GRAINS + NN_REGION_LINE_GRAINS)

/* For each threshold, NN_REGION_THRESHOLD_FIGURES figures: the closes
 * that mark, the objects they entered, the pointers they examined, and
 * then the most bytes held at once at each grain. */
#define NN_REGION_THRESHOLD_CLOSES   0
#define NN_REGION_THRESHOLD_ENTERED  1
#define NN_REGION_THRESHOLD_EXAMINED 2
#define NN_REGION_THRESHOLD_PEAK     3
#define NN_REGION_THRESHOLD_FIGURES  (NN_REGION_THRESHOLD_PEAK + NN_REGION_GRAINS)

/* The tenure collectors, NN_REGION_TENURE_VARIANTS of them, mark at the
 * closes of regions that allocated at least the bytes of this threshold,
 * and NN_REGION_TENURE_FIGURES figures count what each did. */
#define NN_REGION_TENURE_VARIANTS  4
#define NN_REGION_TENURE_THRESHOLD 2
#define NN_REGION_TENURE_CLOSES            0   /* closes that mark */
#define NN_REGION_TENURE_FULL              1   /* of them, those that mark the whole region */
#define NN_REGION_TENURE_ENTERED           2   /* objects the marks entered */
#define NN_REGION_TENURE_EXAMINED          3   /* pointers they examined */
#define NN_REGION_TENURE_RECORDS           4   /* the most records held at once */
#define NN_REGION_TENURE_REMEMBERED        5   /* pointers closes remembered for regions around them */
#define NN_REGION_TENURE_HELD              6   /* the most remembered at once */
#define NN_REGION_TENURE_REACHED           7   /* units and places freed that the close's own mark reached */
#define NN_REGION_TENURE_RESURRECTED       8   /* objects and array entries a later close reached once freed */
#define NN_REGION_TENURE_RESURRECTED_BYTES 9
#define NN_REGION_TENURE_FULL_FREED        10  /* bytes those freed, of objects freed one by one */
#define NN_REGION_TENURE_PEAK              11  /* the most bytes held at once, at each grain */
#define NN_REGION_TENURE_FIGURES           (NN_REGION_TENURE_PEAK + NN_REGION_GRAINS)

/* What is allocated outside every region is what the evaluator does
 * between forces, at the top of an evaluation or in a function it calls
 * there, which no close can free; read with no region open, it counts
 * none still open. */
void nn_region_totals(uint64_t *totals);
void nn_region_classes(uint64_t *figures);     /* NN_REGION_SIZE_CLASSES rows */
void nn_region_thresholds(uint64_t *figures);  /* NN_REGION_THRESHOLDS rows */
void nn_region_tenure(uint32_t variant, uint64_t *figures);  /* NN_REGION_TENURE_FIGURES */

/* The bytes a tenure collector lets closes inside a region tenure before
 * a close marks the region whole, or 0 for none but the outermost. */
uint64_t nn_region_tenure_budget(uint32_t variant);

/* The bytes of threshold k, and of a block or line of a bump kind at
 * grain g from 1. */
uint64_t nn_region_threshold_bytes(uint32_t k);
uint64_t nn_region_grain_bytes(int kind, uint32_t grain);

#endif /* NN_REGION_H */
