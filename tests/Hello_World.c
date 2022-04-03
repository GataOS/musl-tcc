/*
	musl-tcc tests
	src:/tests/Hello_World.c
	Date:2022.04.03
	By MIT License.
	Copyright(c) 2022 Ziyao.All rights reserved.
*/

#include<unistd.h>

int main(void)
{
	write(0,"Hello World\n",12);
	return 0;
}
