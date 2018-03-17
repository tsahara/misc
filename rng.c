#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

uint8_t used[536870912];

uint32_t
xorshift32(uint32_t y)
{
	const uint32_t a = 14, b = 1, c = 15;

	y ^= y << a;
	y ^= y >> b;
	y ^= y << c;
	return y;
}

void
bitcount(uint32_t bits, uint8_t *counters)
{
	int i;

	for (i = 0; i < 32; i++) {
		if (bits & (1UL << i))
			counters[i]++;
	}
}

void
calc_bit_weight(uint32_t a, uint32_t b, uint32_t c)
{
	uint8_t counters[32];
	int i;

	memset(counters, 0, sizeof(counters));
	bitcount(0xffffffff << a, counters);
	bitcount(0xffffffff >> b, counters);
	bitcount(0xffffffff << a, counters);

	printf("(%u,%u,%u) => ", a, b, c);
	for (i = 31; i >= 0; i--) {
		printf("%u", counters[i]);
		if (i % 8 == 0)
			printf(" ");
	}
	printf("\n");
}

int
main(int argc, char **argv)
{
	calc_bit_weight(1,3,10);
	calc_bit_weight(1,5,16);
	calc_bit_weight(5,27,25);
	calc_bit_weight(15,1,29);

	if (0) {
		uint32_t x = 1;
		while (1) {
			printf("%08x\n", x);
			x = xorshift32(x);
		}
	}

	if (0) {
		uint32_t count;
		uint32_t x = 1;

		//x = 0x61114eda;
		for (count = 0; ; count++) {
			unsigned int idx;
			uint8_t bit;

			idx = x / 8;
			bit = 1 << (x % 8);
			if (used[idx] & bit) {
				printf("collide! %08x\n", x);
				exit(1);
			}
			used[idx] |= bit;

			if (1 || (count % 4194304 == 0))
				printf("count=%u, x=%08x\n", count, x);
			x = xorshift32(x);
		}
	}
	return 0;
}
