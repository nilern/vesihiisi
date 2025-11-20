.PHONY: all
all: vesihiisi

vesihiisi: main.c
	cc -o vesihiisi $^


.PHONY: clean
clean:
	rm vesihiisi

