BASER_FLAGS := -Wall -Wextra -Wconversion -fno-strict-aliasing
	# -Wpedantic (using stmt exprs & computed gotos)
BASE_CPP_FLAGS := -std=c++20 $(BASER_FLAGS) -Werror \
	-Ideps/asmjit -DASMJIT_STATIC -DASMJIT_NO_FOREIGN
BASE_C_FLAGS := -std=c23 $(BASER_FLAGS) -Werror
OPT_FLAGS := -O2 -DNDEBUG
DEBUG_FLAGS := -Og -g
SANITIZE_FLAGS := -fsanitize=address -fsanitize=leak -DGC_ALOT

PROD_C_FLAGS := $(BASE_C_FLAGS) $(OPT_FLAGS)
PROD_CPP_FLAGS := $(BASE_CPP_FLAGS) $(OPT_FLAGS)
DEV_C_FLAGS := $(BASE_C_FLAGS) $(DEBUG_FLAGS) # $(SANITIZE_FLAGS)
DEV_CPP_FLAGS := $(BASE_CPP_FLAGS) $(DEBUG_FLAGS) # $(SANITIZE_FLAGS)
TEST_CPP_FLAGS := -std=c++20 $(BASER_FLAGS) $(DEBUG_FLAGS) $(SANITIZE_FLAGS)

LIB_SRCS := $(shell find lib -name '*.[ch]' -o -name '*.[ch]pp')
PROD_LINK_LIBS := -lvesihiisi -lasmjit -lstdc++ # OPTIMIZE: Avoid having to link stdc++
DEV_LINK_LIBS := -lvesihiisi-dev -lasmjit -lstdc++ # OPTIMIZE: Avoid having to link stdc++

.PHONY: all
all: vesihiisi

.PHONY: prod
prod: vesihiisi

.PHONY: dev
dev: vesihiisi-dev

.PHONY: run-dev
run-dev: vesihiisi-dev
	VSHS_HOME=. rlwrap ./vesihiisi-dev -d

.PHONY: debug-dev
debug-dev: vesihiisi-dev
	VSHS_HOME=. gdb --args ./vesihiisi-dev -d

.PHONY: test
test: test/test_heap.out test/test_arena.out test/test_bitset.out test/test_sparsearray.out
	./test/test_heap.out
	./test/test_arena.out
	./test/test_bitset.out
	./test/test_sparsearray.out

vesihiisi: main.c libvesihiisi.a deps/asmjit/build/release/libasmjit.a
	cc $(PROD_C_FLAGS) $< -L. -Ldeps/asmjit/build/release $(PROD_LINK_LIBS) -o $@

libvesihiisi.a: libvesihiisi.o ffi.o
	cd deps/utf8proc; make
	ar -crs $@ $^ deps/utf8proc/utf8proc.o

libvesihiisi.o: $(LIB_SRCS)
	c++ -c $(PROD_CPP_FLAGS) -o $@ lib/vesihiisi.cpp

vesihiisi-dev: main.c libvesihiisi-dev.a deps/asmjit/build/debug/libasmjit.a
	cc $(DEV_C_FLAGS) $< -L. -Ldeps/asmjit/build/debug $(DEV_LINK_LIBS) -o $@

libvesihiisi-dev.a: libvesihiisi-dev.o ffi.o
	cd deps/utf8proc; make
	ar -crs $@ $^ deps/utf8proc/utf8proc.o

libvesihiisi-dev.o: $(LIB_SRCS)
	c++ -c $(DEV_CPP_FLAGS) -o $@ lib/vesihiisi.cpp

ffi.o: lib/ffi.s
	as -o $@ $<

deps/asmjit/build/release/libasmjit.a:
	cd deps/asmjit; ./configure.sh; cmake --build build/release

deps/asmjit/build/debug/libasmjit.a:
	cd deps/asmjit; ./configure.sh; cmake --build build/debug

test/%.out: test/%.cpp $(LIB_SRCS)
	c++ $(TEST_CPP_FLAGS) -o $@ $<

.PHONY: clean
clean:
	rm -f vesihiisi
	rm -f vesihiisi-dev
	rm -f libvesihiisi*
	rm -f test/*.out
	rm -f *.o
