// Defines all the core types used in nucleus
// TODO: These are duplicated in the compiler. Can we combine them somehow?

#ifndef NUC_H_
#define NUC_H_

#include <stdint.h>


#define LOWTAG_BITS 3
#define LOWTAG(x) (((uintptr_t)(x)) & 7)

#define CONS_LOWTAG 2
#define STRING_LOWTAG 4

#define NUC_VAL_TO_INT(x) ((nuc_val)((x) >> LOWTAG_BITS))
#define INT_TO_NUC_VAL(x) ((nuc_val)((x) << LOWTAG_BITS))

typedef uint64_t nuc_val;

typedef struct Cons
{
	nuc_val car;
	nuc_val cdr;
} Cons;

#endif
