#include <assert.h>
#include <string.h>
#include <stdlib.h>
#include "nuc.h"

#define MAX_SYMBOLS 2048

// TODO: Some day this should be a hash table so symbol interning and lookup
// is O(1) rather than O(n).
typedef struct Symbol_table
{
	char *symbols[MAX_SYMBOLS];
} Symbol_table;

static Symbol_table sym_tab;

nuc_val rt_intern_symbol(char *str)
{
	int symbol_pos = -1;
	for (size_t i = 0; i < MAX_SYMBOLS; i++) {
		char **entry = sym_tab.symbols + i;

		if (*entry == 0) {
			// Not found
			// TODO: make a copy?
			*entry = str;
			symbol_pos = i;
			break;
		} else if (strcmp(*entry, str) == 0) {
			// Found it
			symbol_pos = i;
			break;
		}
	}
	assert(symbol_pos != -1);

	return (nuc_val)((symbol_pos << LOWTAG_BITS) | SYMBOL_LOWTAG);
}

char *look_up_symbol(Symbol_table *table, int symbol_index)
{
	char *symbol = table->symbols[symbol_index];
	assert(symbol != NULL);

	return symbol;
}

nuc_val rt_symbol_to_string(nuc_val sym)
{
	CHECK(sym, SYMBOL_TYPE);
	int index = sym >> LOWTAG_BITS;
	char *str = look_up_symbol(&sym_tab, index);

	return rt_make_string(strlen(str), str);
}

nuc_val rt_string_to_symbol(nuc_val str_val)
{
	CHECK(str_val, STRING_TYPE);
	String *str = (String *)REMOVE_LOWTAG(str_val);
	char *c_str = malloc(str->length + 1);
	strncpy(c_str, str->bytes, str->length);
	c_str[str->length] = '\0';

	nuc_val symbol = rt_intern_symbol(c_str);
	free(str);
	return symbol;
}
