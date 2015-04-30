// Defines all the core types used in nucleus
// TODO: These are duplicated in the compiler. Can we combine them somehow?

#ifndef NUC_H_
#define NUC_H_

#include <stdint.h>
#include <stdlib.h>

#define UNREACHABLE assert(!"This should never be reached")

typedef uint64_t nuc_val;
typedef uint32_t Symbol;

// TODO: A lot of these should probably be inline functions.
#define LOWTAG_BITS 3
#define LOWTAG_MASK ((nuc_val)((1 << LOWTAG_BITS) - 1))
#define LOWTAG(x) (((nuc_val)(x)) & LOWTAG_MASK)
#define REMOVE_LOWTAG(x) (((nuc_val)(x)) & ~LOWTAG_MASK)

#define FIXNUM_LOWTAG 0
#define CONS_LOWTAG 2
// TODO: does this really need a lowtag or could we move it to an exttag?
// it's just an integer index into the symbol table, so we probably don't need
// all 61 bits just for that.
#define SYMBOL_LOWTAG 3
#define STRING_LOWTAG 4
#define LAMBDA_LOWTAG 5
#define FOREIGN_LOWTAG 6
#define EXTTAG_LOWTAG 7

#define NUC_VAL_TO_INT(x) ((nuc_val)((x) >> LOWTAG_BITS))
#define INT_TO_NUC_VAL(x) (((nuc_val)(x)) << LOWTAG_BITS)

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
#define SYMBOL_TYPE MAKE_DISCRETE(8)
#define FOREIGN_TYPE MAKE_DISCRETE(9)
#define STRING_TYPE MAKE_DISCRETE(10)
#define LAMBDA_TYPE MAKE_DISCRETE(11)

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


#define CHECK(val, type_tag) \
	rt_check_type(val, type_tag, __FILE__, __func__, __LINE__)

// Functions we want to use across the runtime.
// All the functions that need to be called from the stdlib or have calls
// inserted directly by the compiler have their own declarations in the
// appropriate place, so these are the only ones we need to declare here.
String *rt_make_string(size_t length, char *bytes);
nuc_val rt_cons(nuc_val car, nuc_val cdr);
char *rt_nuc_str_to_c_str(nuc_val nuc_str);
size_t rt_list_length(Cons *cons);
void rt_check_type(nuc_val val, nuc_val type_tag, const char *file, const char *func, int line);

#endif
