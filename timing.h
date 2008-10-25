#ifndef TIMING_H
#define TIMING_H

#if WITH_TIMING

#include <time.h>
#include <stdio.h>

typedef clock_t timing_t;
#define start_timing() clock()

static
void finish_timing(clock_t start, char *info)
{
	clock_t ticks = clock() - start;
	double msecs = ticks/(double)CLOCKS_PER_SEC*1000.0;
	fprintf(stderr, "%s took %g msecs\n", info, msecs);
}

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

#endif


#endif
