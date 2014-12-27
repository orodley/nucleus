// Defines all the core types used in nucleus
// TODO: These are duplicated in the compiler. Can we combine them somehow?

#ifndef NUC_H_
#define NUC_H_

#include <stdint.h>


#define LOWTAG_BITS 3
#define LOWTAG(x) (((uint64_t)(x)) & 7)
#define REMOVE_LOWTAG(x) (nuc_val)(((uintptr_t)(x)) & ~((uint64_t)7))

#define FIXNUM_LOWTAG 0
#define DISCRETE_LOWTAG 1
#define CONS_LOWTAG 2
#define SYMBOL_LOWTAG 3
#define STRING_LOWTAG 4
#define LAMBDA_LOWTAG 5
#define FOREIGN_LOWTAG 6

#define NUC_VAL_TO_INT(x) ((nuc_val)((x) >> LOWTAG_BITS))
#define INT_TO_NUC_VAL(x) ((nuc_val)((x) << LOWTAG_BITS))

#define MAKE_DISCRETE(x) ((nuc_val)(((x) << LOWTAG_BITS) | DISCRETE_LOWTAG))

#define NIL MAKE_DISCRETE(0)
#define TRUE MAKE_DISCRETE(1)
#define FALSE MAKE_DISCRETE(2)
#define FIXNUM_TYPE MAKE_DISCRETE(3)
#define CONS_TYPE MAKE_DISCRETE(4)

typedef uint64_t nuc_val;

typedef struct Cons
{
	nuc_val car;
	nuc_val cdr;
} Cons;

typedef struct String
{
	uint64_t length;
	char bytes[];
} String;

#endif
