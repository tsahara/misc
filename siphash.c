#include <inttypes.h>
#include <stdio.h>
#include <stdlib.h>

void
rol(uint64_t *p, unsigned int count)
{
	uint64_t v = *p;
	*p = (v << count) | (v >> (64 - count));
}

void
sipround(uint64_t *p0, uint64_t *p1, uint64_t *p2, uint64_t *p3)
{
	uint64_t v0 = *p0, v1 = *p1, v2 = *p2, v3 = *p3;

	v0 += v1;
	v2 += v3;
	rol(&v1, 13);
	rol(&v3, 16);
	v1 ^= v0;
	v3 ^= v2;
	rol(&v0, 32);
	v2 += v1;
	v0 += v3;
	rol(&v1, 17);
	rol(&v3, 21);
	v1 ^= v2;
	v3 ^= v0;
	rol(&v2, 32);

	*p0 = v0; *p1 = v1; *p2 = v2; *p3 = v3;
}

uint64_t
siphash_c_d(const char *m, size_t mlen, uint64_t k0, uint64_t k1, int c, int d)
{
	uint64_t mi, v0, v1, v2, v3;
	size_t i, w;
	int j;

	v0 = k0 ^ 0x736f6d6570736575ULL;
	v1 = k1 ^ 0x646f72616e646f6dULL;
	v2 = k0 ^ 0x6c7967656e657261ULL;
	v3 = k1 ^ 0x7465646279746573ULL;

	w = mlen / 8 + 1;

	for (i = 0; i < w; i++) {
		mi = 0;
		if (i < w - 1) {
			for (j = 0; j < 8; j++)
				mi |= (uint64_t)m[i * 8 + j] << (j * 8);
		} else {
			for (j = 0; j < mlen - i * 8; j++)
				mi |= (uint64_t)m[i * 8 + j] << (j * 8);
			mi |= (uint64_t)(mlen % 256) << 56;
		}

		v3 ^= mi;

		for (j = 0; j < c; j++)
			sipround(&v0, &v1, &v2, &v3);

		v0 ^= mi;
	}

	v2 ^= 0xff;

	for (j = 0; j < d; j++)
		sipround(&v0, &v1, &v2, &v3);

	return v0 ^ v1 ^ v2 ^ v3;
}

uint64_t
siphash(const char *m, size_t mlen, uint64_t k0, uint64_t k1)
{
	return siphash_c_d(m, mlen, k0, k1, 2, 4);
}

int
main(int argc, char **argv)
{
	uint64_t h, k0, k1;
	char m[] = {
		0x00, 0x01, 0x02, 0x03, 0x04, 0x05, 0x06, 0x07, 
		0x08, 0x09, 0x0a, 0x0b, 0x0c, 0x0d, 0x0e
	};

	k0 = 0x0706050403020100ULL;
	k1 = 0x0f0e0d0c0b0a0908ULL;
	h = siphash(m, 15, k0, k1);

	printf("siphash = %016llx\n", h);
	exit(0);
}
