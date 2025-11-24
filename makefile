.PHONY: all
all: vesihiisi

vesihiisi: main.c lib/object.c lib/read.c lib/print.c
	cc -std=c2x -Werror -Wall -Wextra -Wpedantic -Wconversion -o $@ $<

.PHONY: clean
clean:
	rm -f vesihiisi

