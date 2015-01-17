#include <assert.h>
#include <stddef.h>
#include <stdio.h>
#include <stdint.h>
#include <string.h>
#include "gc.h"
#include "nuc.h"

typedef struct Lambda
{
	void *function;
	uint8_t arity;
	nuc_val *env[];
} Lambda;

nuc_val rt_make_lambda(void *func_pointer, uint8_t arity,
		uint32_t num_captures, nuc_val **captured_vars)
{
	assert(LOWTAG(func_pointer) == 0);

	Lambda *lambda = gc_alloc(sizeof *lambda + (sizeof(nuc_val *) * num_captures));
	lambda->function = func_pointer;
	lambda->arity = arity;
	memcpy(lambda->env, captured_vars, num_captures * sizeof *captured_vars);

	return ((nuc_val)lambda) | LAMBDA_LOWTAG;
}
