TEST_FLAGS := -std=c2x -Wall -Wextra -Wpedantic -Wconversion
BASE_FLAGS := $(TEST_FLAGS) -Werror
DEBUG_FLAGS := -g
OPT_FLAGS := -O2 -DNDEBUG

LIB_SRCS := $(shell find lib -name '*.c')

.PHONY: all
all: vesihiisi

.PHONY: prod
prod: vesihiisi

.PHONY: dev
dev: vesihiisi-dev

.PHONY: run-dev
run-dev: vesihiisi-dev
	rlwrap ./vesihiisi-dev

.PHONY: test
test: test/test_heap test/test_bitset
	./test/test_heap
	./test/test_bitset

vesihiisi: main.c $(LIB_SRCS)
	cc $(BASE_FLAGS) $(OPT_FLAGS) -o $@ $<

vesihiisi-dev: main.c $(LIB_SRCS)
	cc $(BASE_FLAGS) $(DEBUG_FLAGS) -o $@ $<

test/%: test/%.c $(LIB_SRCS)
	cc $(TEST_FLAGS) $(DEBUG_FLAGS) -o $@ $<

.PHONY: clean
clean:
	rm -f vesihiisi
	rm -f vesihiisi-dev
	rm -f test/test_heap
