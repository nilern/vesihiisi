.PHONY: all
all: prod

.PHONY: prod
prod: main.c lib/util.c lib/object.c lib/heap.c lib/state.c lib/read.c lib/print.c
	cc -std=c2x -Werror -Wall -Wextra -Wpedantic -Wconversion -O2 -o vesihiisi $<

.PHONY: dev
dev: main.c lib/util.c lib/object.c lib/heap.c lib/state.c lib/read.c lib/print.c
	cc -std=c2x -Werror -Wall -Wextra -Wpedantic -Wconversion -g -o vesihiisi $<

.PHONY: test
test: test/test_heap
	./test/test_heap

test/test_heap: test/test_heap.c lib/util.c lib/object.c lib/heap.c lib/state.c
	cc -std=c2x -Wall -Wextra -Wpedantic -Wconversion -g -o $@ $<

.PHONY: clean
clean:
	rm -f vesihiisi
	rm -f test/test_heap

