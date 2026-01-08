BASER_FLAGS := -Wall -Wextra -Wpedantic -Wconversion -fno-strict-aliasing
BASE_CPP_FLAGS := -std=c++20 $(BASER_FLAGS) -Werror
BASE_C_FLAGS := -std=c23 $(BASER_FLAGS) -Werror
OPT_FLAGS := -O2 -DNDEBUG
DEBUG_FLAGS := -g
SANITIZE_FLAGS := -fsanitize=address -fsanitize=leak -DGC_ALOT

PROD_C_FLAGS := $(BASE_C_FLAGS) $(OPT_FLAGS)
PROD_CPP_FLAGS := $(BASE_CPP_FLAGS) $(OPT_FLAGS)
DEV_C_FLAGS := $(BASE_C_FLAGS) $(DEBUG_FLAGS) # $(SANITIZE_FLAGS)
DEV_CPP_FLAGS := $(BASE_CPP_FLAGS) $(DEBUG_FLAGS) # $(SANITIZE_FLAGS)
TEST_CPP_FLAGS := -std=c++20 $(BASER_FLAGS) $(DEBUG_FLAGS) $(SANITIZE_FLAGS)

LIB_SRCS := $(shell find lib -name '*.[ch]' -o -name '*.[ch]pp')
PROD_LINK_LIBS := -lvesihiisi -lstdc++ # OPTIMIZE: Avoid having to link stdc++ (only using a `delete`)
DEV_LINK_LIBS := -lvesihiisi-dev -lstdc++ # OPTIMIZE: Avoid having to link stdc++ (only using a `delete`)

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
test: test/test_heap.out test/test_arena.out test/test_bitset.out test/test_sparsearray.out
	./test/test_heap.out
	./test/test_arena.out
	./test/test_bitset.out
	./test/test_sparsearray.out

vesihiisi: main.c libvesihiisi.a
	cc $(PROD_C_FLAGS) $< -L. $(PROD_LINK_LIBS) -o $@

libvesihiisi.o: $(LIB_SRCS)
	c++ -c $(PROD_CPP_FLAGS) -o $@ lib/vesihiisi.cpp

vesihiisi-dev: main.c libvesihiisi-dev.a
	cc $(DEV_C_FLAGS) $< -L. $(DEV_LINK_LIBS) -o $@

libvesihiisi-dev.o: $(LIB_SRCS)
	c++ -c $(DEV_CPP_FLAGS) -o $@ lib/vesihiisi.cpp

%.a: %.o
	ar rcs $@ $<

test/%.out: test/%.cpp $(LIB_SRCS)
	c++ $(TEST_CPP_FLAGS) -o $@ $<

.PHONY: clean
clean:
	rm -f vesihiisi
	rm -f vesihiisi-dev
	rm -f libvesihiisi*
	rm -f test/*.out
