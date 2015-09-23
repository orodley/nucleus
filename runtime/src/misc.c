#define _POSIX_C_SOURCE 199309
#include <assert.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/time.h>
#include <time.h>
#include "gc.h"
#include "nuc.h"

nuc_val rt_panic(nuc_val message)
{
	String *str = (String *)REMOVE_LOWTAG(message);
	fprintf(stderr, "Panic!: %.*s\n", (int)str->length, str->bytes);

	exit(1);
}

static nuc_val argv_list;

void rt_store_argv(int32_t argc, char *argv[])
{
	if (argc == 0) {
		argv_list = NIL;
		return;
	}

	Cons *c = gc_alloc(sizeof *c);
	c->car = ((nuc_val)rt_make_string(strlen(argv[0]), argv[0])) | STRING_LOWTAG;
	argv_list = ((nuc_val)c) | CONS_LOWTAG;
	for (int32_t i = 1; i < argc; i++) {
		Cons *cdr = gc_alloc(sizeof *cdr);
		c->cdr = ((nuc_val)cdr) | CONS_LOWTAG;
		c = cdr;
		c->car = ((nuc_val)rt_make_string(strlen(argv[i]), argv[i])) | STRING_LOWTAG;
	}

	c->cdr = NIL;
}

nuc_val rt_get_argv()
{
	return argv_list;
}

static bool seeded_rng = false;

nuc_val rt_rand()
{
	if (!seeded_rng) {
		struct timeval t;
		gettimeofday(&t, NULL);
		srand(t.tv_usec * t.tv_sec);

		seeded_rng = true;
	}

	return INT_TO_NUC_VAL(rand());
}

uint64_t rt_get_nanoseconds()
{
	struct timespec t;
	clock_gettime(CLOCK_MONOTONIC, &t);

	return (uint64_t)t.tv_sec * 1000000000 + t.tv_nsec;
}


void rt_raw_print(nuc_val val)
{
	printf("0x%016lx\n", val);
}
