#ifndef TIMING_H
#define TIMING_H

#if WITH_TIMING

#include <time.h>
#include <sys/time.h>
#include <stdio.h>

static
long long gpicker_hrtime_micros()
{
	struct timeval tv;
	gettimeofday(&tv, 0);
	return tv.tv_sec * 1000000LL + tv.tv_usec;
}

typedef clock_t timing_t;
#define start_timing() gpicker_hrtime_micros()

static
void finish_timing(clock_t start, char *info)
{
	long long ticks = gpicker_hrtime_micros() - start;
	double msecs = ticks/1000.0;
	fprintf(stderr, "%s took %g msecs\n", info, msecs);
}

#define timing_printf(...) fprintf(stderr, __VA_ARGS__)

#else

typedef int timing_t;
static inline
timing_t start_timing()
{
	return 0;
}

static inline
void finish_timing(timing_t start, char *info)
{
}


static inline __attribute__((format (printf, 1, 2))) __attribute__((unused))
int timing_printf(const char *p, ...)
{
	return 0;
}

#endif


#endif
