.PHONY: all
all: vesihiisi

vesihiisi: main.c lib/object.c
	cc -std=c2x -Werror -Wall -Wextra -Wpedantic -Wconversion -o $@ $<

.PHONY: clean
clean:
	rm -f vesihiisi

