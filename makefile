TEST_FLAGS := -std=c2x -Wall -Wextra -Wpedantic -Wconversion -fno-strict-aliasing
BASE_FLAGS := $(TEST_FLAGS) -Werror
DEBUG_FLAGS := -g # -fsanitize=address -fsanitize=leak -DGC_ALOT
OPT_FLAGS := -O2 -DNDEBUG

LIB_SRCS := $(shell find lib -name '*.[ch]')

.PHONY: all
all: vesihiisi

.PHONY: prod
prod: vesihiisi

.PHONY: dev
dev: vesihiisi-dev

.PHONY: run-dev
run-dev: vesihiisi-dev
	rlwrap ./vesihiisi-dev -di base/bootstrap.lisp

.PHONY: test
test: test/test_heap.out test/test_arena.out test/test_bitset.out
	./test/test_heap.out
	./test/test_arena.out
	./test/test_bitset.out

vesihiisi: main.c libvesihiisi.a
	cc $(BASE_FLAGS) $(OPT_FLAGS) $< -L. -lvesihiisi -o $@

libvesihiisi.o: $(LIB_SRCS)
	cc -c $(BASE_FLAGS) $(OPT_FLAGS) -o $@ lib/vesihiisi.c

vesihiisi-dev: main.c libvesihiisi-dev.a
	cc $(BASE_FLAGS) $(DEBUG_FLAGS) $< -L. -lvesihiisi-dev -o $@

libvesihiisi-dev.o: $(LIB_SRCS)
	cc -c $(BASE_FLAGS) $(DEBUG_FLAGS) -o $@ lib/vesihiisi.c

%.a: %.o
	ar rcs $@ $<

test/%.out: test/%.c $(LIB_SRCS)
	cc $(TEST_FLAGS) $(DEBUG_FLAGS) -o $@ $<

.PHONY: clean
clean:
	rm -f vesihiisi
	rm -f vesihiisi-dev
	rm -f libvesihiisi*
	rm -f test/*.out
