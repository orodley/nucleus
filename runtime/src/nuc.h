// Defines all the core types used in nucleus
// TODO: These are duplicated in the compiler. Can we combine them somehow?

#ifndef NUC_H_
#define NUC_H_

#include <stdint.h>

typedef uint64_t nuc_val;

#define LOWTAG_BITS 3
#define LOWTAG_MASK ((nuc_val)((1 << LOWTAG_BITS) - 1))
#define LOWTAG(x) (((nuc_val)(x)) & LOWTAG_MASK)
#define REMOVE_LOWTAG(x) (((nuc_val)(x)) & ~LOWTAG_MASK)

#define FIXNUM_LOWTAG 0
#define CONS_LOWTAG 2
#define SYMBOL_LOWTAG 3
#define STRING_LOWTAG 4
#define LAMBDA_LOWTAG 5
#define FOREIGN_LOWTAG 6
#define EXTTAG_LOWTAG 7

// TODO: these should probably be inline functions.
#define NUC_VAL_TO_INT(x) ((nuc_val)((x) >> LOWTAG_BITS))
#define INT_TO_NUC_VAL(x) ((nuc_val)((x) << LOWTAG_BITS))
#define EXTTAG_BITS 5
#define EXTTAG_MASK ((nuc_val)((1 << (EXTTAG_BITS - 1)) - 1))
#define EXTTAG(x) ((((nuc_val)(x)) >> LOWTAG_BITS) & EXTTAG_MASK)
#define REMOVE_EXTTAG(x) ((((nuc_val)(x)) >> LOWTAG_BITS) & ~EXTTAG_MASK)

#define DISCRETE_EXTTAG 0
#define FLOAT_EXTTAG 1

#define MAKE_DISCRETE(x) \
	((nuc_val)(((x) << (LOWTAG_BITS + EXTTAG_BITS)) \
		| (DISCRETE_EXTTAG << LOWTAG_BITS) \
		| EXTTAG_LOWTAG))

#define NIL MAKE_DISCRETE(0)
#define TRUE MAKE_DISCRETE(1)
#define FALSE MAKE_DISCRETE(2)
#define FIXNUM_TYPE MAKE_DISCRETE(3)
#define CONS_TYPE MAKE_DISCRETE(4)
#define NIL_TYPE MAKE_DISCRETE(5)
#define BOOL_TYPE MAKE_DISCRETE(6)
#define FLOAT_TYPE MAKE_DISCRETE(7)

typedef union Float_converter
{
	uint32_t as_int;
	float as_float;
} Float_converter;

inline nuc_val float_to_nuc_val(float f)
{
	Float_converter fc;
	fc.as_float = f;
	return ((uint64_t)fc.as_int << 32)
		| (FLOAT_EXTTAG << LOWTAG_BITS)
		| EXTTAG_LOWTAG;
}

inline float nuc_val_to_float(nuc_val x)
{
	Float_converter fc;
	fc.as_int = x >> 32;
	return fc.as_float;
}

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
