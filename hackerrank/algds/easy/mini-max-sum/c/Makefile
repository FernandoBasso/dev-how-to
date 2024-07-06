CLIBS=$(HOME)/local/clibs
CC=gcc
CFLAGS=-std=c99 \
	-Og \
	-g3 \
	-Wall \
	-Wextra \
	-Wpedantic \
	-fsanitize=address,undefined \
	-I $(CLIBS)/criterion/include \
	-L $(CLIBS)/criterion/lib \
	-l criterion

./out:
	mkdir -pv ./out

./out/%: %.c
	$(CC) $(CFLAGS) $< -o $@

clean:
	rm -rfv ./out/*

