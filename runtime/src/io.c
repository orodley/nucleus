#include <assert.h>
#include <stdio.h>
#include <string.h>
#include "gc.h"
#include "nuc.h"

typedef struct Stream
{
	enum
	{
		OS, STRING
	} type;
	union
	{
		FILE *os_stream;
		struct
		{
			char *chars;
			size_t capacity;
			size_t used;
		} string;
	} impl;
} Stream;

nuc_val rt_write_char_to_stream(nuc_val stream_val, nuc_val char_val)
{
	CHECK(stream_val, FOREIGN_LOWTAG);
	CHECK(char_val, FIXNUM_LOWTAG);
	Stream *stream = (Stream *)REMOVE_LOWTAG(stream_val);
	char c = (char)NUC_VAL_TO_INT(char_val);

	switch (stream->type) {
	case OS:
		fputc(c, stream->impl.os_stream);
		break;
	default:
		assert(!"Not implemented");
	}

	return c;
}

nuc_val rt_write_string_to_stream(nuc_val stream_val, nuc_val string_val)
{
	CHECK(stream_val, FOREIGN_LOWTAG);
	CHECK(string_val, STRING_LOWTAG);
	Stream *stream = (Stream *)REMOVE_LOWTAG(stream_val);
	String *string = (String *)REMOVE_LOWTAG(string_val);

	switch (stream->type) {
	case OS:
		fwrite(string->bytes, 1, string->length, stream->impl.os_stream);
		break;
	default:
		assert(!"Not implemented");
	}

	return string_val;
}

// TODO: move to stdlib
nuc_val rt_write_string_ln_to_stream(nuc_val stream, nuc_val string)
{
	rt_write_string_to_stream(stream, string);
	rt_write_char_to_stream(stream, INT_TO_NUC_VAL('\n'));

	return string;
}

nuc_val rt_read_char_from_stream(nuc_val stream_val)
{
	CHECK(stream_val, FOREIGN_LOWTAG);
	Stream *stream = (Stream *)REMOVE_LOWTAG(stream_val);

	switch (stream->type) {
	case OS:
		return INT_TO_NUC_VAL(fgetc(stream->impl.os_stream));
	default:
		assert(!"Not implemented");
	}
}

static nuc_val os_stream_to_stream(FILE *os_stream)
{
	Stream *stream = gc_alloc(sizeof *stream);
	stream->type = OS;
	stream->impl.os_stream = os_stream;

	return ((nuc_val)stream) | FOREIGN_LOWTAG;
}

nuc_val rt_get_stdin()
{
	return os_stream_to_stream(stdin);
}

nuc_val rt_get_stdout()
{
	return os_stream_to_stream(stdout);
}

nuc_val rt_open(nuc_val string)
{
	char *filename = rt_nuc_str_to_c_str(string);
	return os_stream_to_stream(fopen(filename, "r"));
}

nuc_val rt_close(nuc_val stream_val)
{
	CHECK(stream_val, FOREIGN_LOWTAG);
	Stream *stream = (Stream *)REMOVE_LOWTAG(stream_val);

	switch (stream->type) {
	case OS:
		fclose(stream->impl.os_stream);
	default:
		assert(!"Not implemented");
	}

	return NIL;
}
