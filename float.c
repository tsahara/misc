#include <stdio.h>
#include <stdint.h>
#include <math.h>

double
make_double(unsigned int sign, unsigned int exp, uint64_t frac)
{
	double d = 0.0;
	uint64_t q;

	q = (uint64_t)(sign & 1) << 63;
	q += (uint64_t)exp << 52;
	q += frac;
	*(uint64_t *)&d = q;
	return d;
}

int
main(int argc, char **argv)
{
	double d;
	int i, exponent;
	uint64_t q;
	uint8_t *cp;

	d = 1;
	d = make_double(0, 1020, 1);
	q = 0;
	cp = (uint8_t *)&d;

	printf("double %.54g\n", d);

	printf("bytes: ");
	for (i = 7; i >= 0; i--) {
		printf("%02x ", cp[i]);
		q += (uint64_t)cp[i] << (i * 8);
	}
	printf("\n");

	printf("sign=%llu, ", q >> 63);
	exponent = (q >> 52) & 0x3ff;
	printf("exp=%u(%d), ", exponent, exponent - 1023);
	d = fabs(d);
	printf("frac=%.18f\n", d - 1.0);
	return 0;
}
