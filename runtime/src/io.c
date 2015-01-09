#include <assert.h>
#include <stdio.h>
#include <string.h>
#include "gc.h"
#include "nuc.h"

typedef struct String_stream
{
	char *chars;
	size_t capacity;
	size_t read_pos;
	size_t write_pos;
} String_stream;

typedef struct Stream
{
	enum
	{
		OS, STRING
	} type;
	union
	{
		FILE *os_stream;
		String_stream string_stream;
	} impl;
} Stream;

static void string_stream_ensure_room_for(String_stream *stream, size_t size);


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
	case STRING:
	{
		String_stream *str_stream = &stream->impl.string_stream;
		string_stream_ensure_room_for(str_stream, 1);
		str_stream->chars[str_stream->write_pos++] = c;
		break;
	}
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
	case STRING:
	{
		String_stream *str_stream = &stream->impl.string_stream;
		string_stream_ensure_room_for(str_stream, string->length);
		strncpy(str_stream->chars + str_stream->write_pos,
				string->bytes,
				string->length);
		str_stream->write_pos += string->length;
		break;
	}
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
	case STRING:
	{
		String_stream *str_stream = &stream->impl.string_stream;
		if (str_stream->read_pos == str_stream->write_pos)
			return EOF;
		else
			return INT_TO_NUC_VAL(str_stream->chars[str_stream->read_pos++]);
	}
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
		break;
	case STRING:
		free(stream->impl.string_stream.chars);
		break;
	default:
		assert(!"Not implemented");
	}

	return NIL;
}

// TODO: We should probably use the string stream buffer as a circular buffer

#define MINIMUM_INITIAL_STRING_STREAM_SIZE 32
#define STRING_STREAM_GROWTH_FACTOR 2

nuc_val rt_make_string_stream(nuc_val initial_contents_val)
{
	CHECK(initial_contents_val, STRING_LOWTAG);
	String *initial_contents = (String *)REMOVE_LOWTAG(initial_contents_val);

	Stream *stream = gc_alloc(sizeof *stream);
	stream->type = STRING;

	size_t initial_size = initial_contents->length * STRING_STREAM_GROWTH_FACTOR;
	if (initial_size < MINIMUM_INITIAL_STRING_STREAM_SIZE)
		initial_size = MINIMUM_INITIAL_STRING_STREAM_SIZE;

	String_stream *str_stream = &stream->impl.string_stream;
	str_stream->chars = malloc(initial_size);
	strncpy(str_stream->chars, initial_contents->bytes, initial_contents->length);
	str_stream->capacity = initial_size;
	str_stream->read_pos = 0;
	str_stream->write_pos = initial_contents->length;

	return (nuc_val)stream | FOREIGN_LOWTAG;
}

nuc_val rt_string_stream_to_string(nuc_val stream_val)
{
	CHECK(stream_val, FOREIGN_LOWTAG);
	Stream *stream = (Stream *)REMOVE_LOWTAG(stream_val);
	String_stream *string_stream = &stream->impl.string_stream;

	return rt_make_string(string_stream->write_pos - string_stream->read_pos,
			string_stream->chars + string_stream->read_pos);
}

static void string_stream_ensure_room_for(String_stream *stream, size_t size)
{
	int diff = stream->write_pos + size - stream->capacity;
	if (diff > 0)
		return;

	size_t required_size = ((stream->write_pos - stream->read_pos) + size);
	size_t new_buf_size = required_size * STRING_STREAM_GROWTH_FACTOR;

	char *new_buf;
	if (new_buf_size > stream->capacity)
		new_buf = malloc(new_buf_size);
	else
		new_buf = stream->chars;

	memmove(new_buf, stream->chars + stream->read_pos, required_size);
	stream->capacity = new_buf_size;
	stream->write_pos = stream->write_pos - stream->read_pos;
	stream->read_pos = 0;
	stream->chars = new_buf;
}
