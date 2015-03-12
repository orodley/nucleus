CC ?= clang
CXX ?= clang++
CFLAGS := -ggdb3 -std=c99 -Wall -Wextra -Wstrict-prototypes
CFLAGS += $(shell llvm-config --cflags)
LD_FLAGS := $(shell llvm-config --ldflags --system-libs --libs core analysis bitreader support)

.PHONY: all
all: llvm-cfg

llvm-cfg: llvm-cfg.o
	$(CXX) $< $(LD_FLAGS) -o $@

.PHONY: clean
clean:
	rm -f *.o
	rm -f llvm-cfg
