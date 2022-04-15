#!/bin/sh

# Compiling one C source with tcc and musl
# tcc-musl tests
# file: /tests/Compile.sh
# Date: 2022.04.03

tcc $1 -nostdlib ../lib/Scrt1.o -o $2 -Wall -Werror -g -O0		\
	-pedantic -Wextra -I ../include -I ../obj/include		\
	-I ../arch/x86_64/ -I ../arch/generic ldscript.ld -nostdinc
