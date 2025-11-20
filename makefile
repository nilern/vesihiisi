.PHONY: all
all: vesihiisi

vesihiisi: main.c
	cc -std=c2x -Werror -Wall -Wextra -Wpedantic -Wconversion -o vesihiisi $^

.PHONY: clean
clean:
	rm -f vesihiisi

