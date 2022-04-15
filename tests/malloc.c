#include<assert.h>
#include<stdio.h>
#include<stdlib.h>
#include<stdint.h>
#include<string.h>

#define BLK_SIZE (2 << 10)
#define TEST_NUM (2 << 10)

int main(void)
{
	uint8_t **slots = (uint8_t**)malloc(sizeof(uint8_t*) * TEST_NUM);
	printf("%p\n",slots);
	assert(slots);
	for (int i = 0;i < TEST_NUM;i++) {
		slots[i] = (uint8_t*)malloc(BLK_SIZE);
		assert(slots[i]);
		memset(slots[i],(uint8_t)i,BLK_SIZE);
	}

	for (int i = 0;i < TEST_NUM;i++)
		free(slots[i]);
	
	return 0;
}
