CFLAGS = -std=c99 -O3 -Wall -Wextra -Wstrict-prototypes -Werror -Isrc
BC = $(patsubst %.c, %.bc, $(wildcard src/*.c))

.PHONY: all clean

all: nuc-runtime.bc

nuc-runtime.bc: $(BC)
	llvm-link-3.5 $^ -o $@

%.bc: %.c
	clang $(CFLAGS) -c -emit-llvm $< -o $@

clean:
	rm -f src/*.bc
