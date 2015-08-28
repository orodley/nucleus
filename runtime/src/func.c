#include <assert.h>
#include <stddef.h>
#include <stdint.h>
#include <stdio.h>
#include <string.h>
#include "gc.h"
#include "nuc.h"

typedef struct Lambda
{
	void *function;
	uint8_t arity;
	nuc_val *env[];
} Lambda;

Lambda *rt_make_lambda(void *func_pointer, uint8_t arity,
		uint32_t num_captures, nuc_val **captured_vars)
{
	assert(LOWTAG(func_pointer) == 0);

	Lambda *lambda;
	if (num_captures == 0) {
		lambda = gc_alloc(sizeof *lambda + sizeof(nuc_val *));
		*lambda->env = NULL;
	} else {
		lambda = gc_alloc(sizeof *lambda + (sizeof(nuc_val *) * num_captures));
		memcpy(&lambda->env, captured_vars, num_captures * sizeof *captured_vars);
	}
	lambda->function = func_pointer;
	lambda->arity = arity;

	return lambda;
}

void rt_check_arity(Lambda *lambda, int expected_arity)
{
	if (expected_arity != lambda->arity) {
		fprintf(stderr, "Wrong number of arguments to function: "
				"got %d, expected %d\n", lambda->arity, expected_arity);
		exit(1);
	}
}
