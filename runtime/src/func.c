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
	uint32_t num_captures;
	nuc_val *env[];
} Lambda;

Lambda *rt_make_lambda(void *func_pointer, uint8_t arity,
		uint32_t num_captures, nuc_val **captured_vars)
{
	Lambda *lambda = gc_alloc(sizeof *lambda + (sizeof(nuc_val *) * num_captures));
	memcpy(&lambda->env, captured_vars, num_captures * sizeof *captured_vars);
	lambda->function = func_pointer;
	lambda->arity = arity;
	lambda->num_captures = num_captures;

	return lambda;
}

Lambda *rt_proxy_for_lambda(Lambda *lambda, void *replacement_func_pointer)
{
	Lambda *proxy = gc_alloc(
			sizeof *proxy + sizeof(nuc_val *) * (lambda->num_captures + 1));
	memcpy(&proxy->env, &lambda->env, lambda->num_captures * sizeof(nuc_val *));
	proxy->env[lambda->num_captures] = (nuc_val *)lambda->function;
	proxy->function = replacement_func_pointer;
	proxy->arity = lambda->arity;
	proxy->num_captures = lambda->num_captures + 1;

	return proxy;
}

void rt_check_arity(Lambda *lambda, int expected_arity)
{
	if (expected_arity != lambda->arity) {
		fprintf(stderr, "Wrong number of arguments to function: "
				"got %d, expected %d\n", lambda->arity, expected_arity);
		exit(1);
	}
}
