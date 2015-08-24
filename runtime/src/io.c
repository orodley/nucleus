#include <assert.h>
#include <errno.h>
#include <inttypes.h>
#include <stdio.h>
#include <string.h>
#include "gc.h"
#include "nuc.h"

#define NOT_IMPLEMENTED assert(!"Not implemented")

typedef struct String_stream
{
	char *chars;
	size_t capacity;
	size_t read_pos;
	size_t write_pos;
} String_stream;

// TODO: we should probably have a separate tag for streams.

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

	// Used to support limited backtracking - the nucleus parser only needs
	// one character worth.
	char unread_char;

	char *source_name;
	int current_line;
	int current_col;
} Stream;

nuc_val rt_stream_get_source_name(nuc_val stream_val)
{
	CHECK(stream_val, FOREIGN_TYPE);
	Stream *stream = (Stream *)REMOVE_LOWTAG(stream_val);

	char *str = stream->source_name;
	return ((nuc_val)(rt_make_string(strlen(str), str))) | STRING_LOWTAG;
}

nuc_val rt_stream_get_line(nuc_val stream_val)
{
	CHECK(stream_val, FOREIGN_TYPE);
	Stream *stream = (Stream *)REMOVE_LOWTAG(stream_val);

	return INT_TO_NUC_VAL(stream->current_line);
}

nuc_val rt_stream_get_col(nuc_val stream_val)
{
	CHECK(stream_val, FOREIGN_TYPE);
	Stream *stream = (Stream *)REMOVE_LOWTAG(stream_val);

	return INT_TO_NUC_VAL(stream->current_col);
}

static void string_stream_ensure_room_for(String_stream *stream, size_t size);


nuc_val rt_write_char_to_stream(nuc_val stream_val, nuc_val char_val)
{
	CHECK(stream_val, FOREIGN_TYPE);
	CHECK(char_val, FIXNUM_TYPE);
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
		NOT_IMPLEMENTED;
	}

	return c;
}

static void write_bytes_to_stream(Stream *stream, char *bytes, size_t length);

nuc_val rt_write_string_to_stream(nuc_val stream_val, nuc_val string_val)
{
	CHECK(stream_val, FOREIGN_TYPE);
	CHECK(string_val, STRING_TYPE);
	Stream *stream = (Stream *)REMOVE_LOWTAG(stream_val);
	String *string = (String *)REMOVE_LOWTAG(string_val);

	write_bytes_to_stream(stream, string->bytes, string->length);
	return string_val;
}

static void write_bytes_to_stream(Stream *stream, char *bytes, size_t length)
{
	switch (stream->type) {
	case OS:
		fwrite(bytes, 1, length, stream->impl.os_stream);
		break;
	case STRING:
	{
		String_stream *str_stream = &stream->impl.string_stream;
		string_stream_ensure_room_for(str_stream, length);
		strncpy(str_stream->chars + str_stream->write_pos, bytes, length);
		str_stream->write_pos += length;
		break;
	}
	default:
		NOT_IMPLEMENTED;
	}
}

// TODO: move to stdlib
nuc_val rt_write_string_ln_to_stream(nuc_val stream, nuc_val string)
{
	rt_write_string_to_stream(stream, string);
	rt_write_char_to_stream(stream, INT_TO_NUC_VAL('\n'));

	return string;
}

nuc_val rt_write_addr_to_stream(nuc_val stream_val, nuc_val obj)
{
	CHECK(stream_val, FOREIGN_TYPE);
	CHECK(obj, FOREIGN_TYPE);
	Stream *stream = (Stream *)REMOVE_LOWTAG(stream_val);
	uintptr_t foreign_ptr = *(uintptr_t *)REMOVE_LOWTAG(obj);

#define SIZE (sizeof("#<foreign 0x0000000000000000>"))
	char fmt[] = "#<foreign 0x%016" PRIxPTR ">";
	char out[SIZE];
	snprintf(out, SIZE, fmt, foreign_ptr);

	write_bytes_to_stream(stream, out, SIZE - 1);
#undef SIZE

	return NIL;
}

nuc_val rt_read_char_from_stream(nuc_val stream_val)
{
	CHECK(stream_val, FOREIGN_TYPE);
	Stream *stream = (Stream *)REMOVE_LOWTAG(stream_val);

	if (stream->unread_char != '\0') {
		char c = stream->unread_char;
		stream->unread_char = '\0';

		return INT_TO_NUC_VAL(c);
	}

	char c;
	switch (stream->type) {
	case OS:
		c = fgetc(stream->impl.os_stream);

		break;
	case STRING:
	{
		String_stream *str_stream = &stream->impl.string_stream;
		if (str_stream->read_pos == str_stream->write_pos)
			c = EOF;
		else
			c = str_stream->chars[str_stream->read_pos++];

		break;
	}
	default:
		NOT_IMPLEMENTED;
	}

	if (c == '\n') {
		stream->current_line++;

		// We start at 0 here, as the first character of a line is the one
		// after the newline.
		stream->current_col = 0;
	} else {
		stream->current_col++;
	}

	return INT_TO_NUC_VAL(c);
}

nuc_val rt_unread_char(nuc_val stream_val, nuc_val char_val)
{
	CHECK(stream_val, FOREIGN_TYPE);
	CHECK(char_val, FIXNUM_TYPE);
	Stream *stream = (Stream *)REMOVE_LOWTAG(stream_val);
	char c = NUC_VAL_TO_INT(char_val);

	assert(stream->unread_char == '\0');

	stream->unread_char = c;

	return NIL;
}

static nuc_val os_stream_to_stream(FILE *os_stream, char *file_name)
{
	Stream *stream = gc_alloc(sizeof *stream);
	stream->type = OS;
	stream->impl.os_stream = os_stream;
	stream->unread_char = '\0';
	stream->source_name = file_name;
	stream->current_line = 1;
	stream->current_col = 0;

	return ((nuc_val)stream) | FOREIGN_LOWTAG;
}

nuc_val rt_get_stdin()
{
	return os_stream_to_stream(stdin, "<stdin>");
}

nuc_val rt_get_stdout()
{
	return os_stream_to_stream(stdout, "<stdout>");
}

nuc_val rt_get_stderr()
{
	return os_stream_to_stream(stderr, "<stderr>");
}

nuc_val rt_open(nuc_val string)
{
	char *filename = rt_nuc_str_to_c_str(string);
	FILE *fp = fopen(filename, "r");
	if (fp == NULL) {
		int open_err = errno;
		fprintf(stderr, "Panic!: Error opening file \"%s\": ", filename);
		errno = open_err;
		perror("");

		exit(1);
	}
	return os_stream_to_stream(fp, filename);
}

nuc_val rt_close(nuc_val stream_val)
{
	CHECK(stream_val, FOREIGN_TYPE);
	Stream *stream = (Stream *)REMOVE_LOWTAG(stream_val);

	switch (stream->type) {
	case OS:
		fclose(stream->impl.os_stream);
		break;
	case STRING:
		free(stream->impl.string_stream.chars);
		break;
	default:
		NOT_IMPLEMENTED;
	}

	return NIL;
}

// TODO: We should probably use the string stream buffer as a circular buffer

#define MINIMUM_INITIAL_STRING_STREAM_SIZE 4
#define STRING_STREAM_GROWTH_FACTOR 2

nuc_val rt_make_string_stream(nuc_val initial_contents_val)
{
	CHECK(initial_contents_val, STRING_TYPE);
	String *initial_contents = (String *)REMOVE_LOWTAG(initial_contents_val);

	Stream *stream = gc_alloc(sizeof *stream);
	stream->type = STRING;
	stream->unread_char = '\0';
	stream->source_name = "<string stream>";
	stream->current_line = 1;
	stream->current_col = 1;

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
	CHECK(stream_val, FOREIGN_TYPE);
	Stream *stream = (Stream *)REMOVE_LOWTAG(stream_val);
	String_stream *string_stream = &stream->impl.string_stream;

	return ((nuc_val)rt_make_string(string_stream->write_pos - string_stream->read_pos,
			string_stream->chars + string_stream->read_pos)) | STRING_LOWTAG;
}

static void string_stream_ensure_room_for(String_stream *stream, size_t size)
{
	if (stream->write_pos + size < stream->capacity)
		return;

	size_t required_size = ((stream->write_pos - stream->read_pos) + size);
	size_t new_buf_size = required_size * STRING_STREAM_GROWTH_FACTOR;

	char *new_buf;
	if (new_buf_size > stream->capacity)
		new_buf = malloc(new_buf_size);
	else
		new_buf = stream->chars;

	memmove(new_buf, stream->chars + stream->read_pos, required_size);
	if (new_buf != stream->chars)
		free(stream->chars);

	stream->capacity = new_buf_size;
	stream->write_pos = stream->write_pos - stream->read_pos;
	stream->read_pos = 0;
	stream->chars = new_buf;

	assert(stream->write_pos + size < stream->capacity);
}
