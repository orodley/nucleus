#include <assert.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>
#include "gc.h"
#include "nuc.h"

nuc_val rt_panic(nuc_val message)
{
	String *str = (String *)REMOVE_LOWTAG(message);
	fputs("(panic! \"", stderr);
	fwrite(str->bytes, 1, str->length, stderr);
	fputs("\")\n", stderr);

	exit(-1);
}

static nuc_val argv_list;

void rt_store_argv(int32_t argc, char *argv[])
{
	if (argc == 0) {
		argv_list = NIL;
		return;
	}

	Cons *c = gc_alloc(sizeof *c);
	c->car = rt_make_string(strlen(argv[0]), argv[0]);
	argv_list = ((nuc_val)c) | CONS_LOWTAG;
	for (int32_t i = 1; i < argc; i++) {
		Cons *cdr = gc_alloc(sizeof *cdr);
		c->cdr = ((nuc_val)cdr) | CONS_LOWTAG;
		c = cdr;
		c->car = rt_make_string(strlen(argv[i]), argv[i]);
	}

	c->cdr = NIL;
}

nuc_val rt_get_argv()
{
	return argv_list;
}

nuc_val rt_get_stdin()
{
	assert(LOWTAG(stdin) == 0);
	return ((nuc_val)stdin) | FOREIGN_LOWTAG;
}
