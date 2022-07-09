#!/bin/sh

# Compiling one C source with tcc and musl
# tcc-musl tests
# file: /tests/Compile.sh
# Date: 2022.04.03

export LD_SO="../lib/libc.so"
tcc $1 -nostdlib  ../lib/libc.so -o $2			\
	-Wall -Werror -g -O0 ../lib/Scrt1.o \
	-pedantic -Wextra -I ../include -I ../obj/include		\
	-I ../arch/x86_64/ -I ../arch/generic -nostdinc			\
