#include <stdint.h>
#include <stdio.h>

/*
 * John Lamping, Eric Veach, "A Fast, Minimal Memory, Consistent Hash
 * Algorithm"
 */
int32_t
jchash(uint64_t key, int32_t num_buckets)
{
	int64_t b = -1, j = 0;
	while (j < num_buckets) {
		b = j;
		key = key * 2862933555777941757ULL + 1;
		j = (b + 1) * ((double)(1LL << 31) / (double)((key >> 33) + 1));
	}
	return b;
}

int
main(int argc, char **argv)
{
	uint64_t key = 2;
	int i;

	for (i = 0; i < 10; i++) {
		printf("[%d] = %d\n", i, jchash(key, i));
	}
	return 0;
}
