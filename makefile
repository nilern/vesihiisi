.PHONY: all
all: vesihiisi

vesihiisi: main.c lib/object.c lib/heap.c lib/read.c lib/print.c
	cc -std=c2x -Werror -Wall -Wextra -Wpedantic -Wconversion -o $@ $<

.PHONY: test
test: test/test_heap
	./test/test_heap

test/test_heap: test/test_heap.c lib/heap.c
	cc -std=c2x -Werror -Wall -Wextra -Wpedantic -Wconversion -g -o $@ $<

.PHONY: clean
clean:
	rm -f vesihiisi
	rm -f test/test_heap

