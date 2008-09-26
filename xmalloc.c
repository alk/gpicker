#include <stdio.h>
#include <stdlib.h>
#include "xmalloc.h"

__attribute__((noreturn))
void memory_exhausted(void)
{
	fputs("memory exhausted", stderr);
	abort();
}

void *xmalloc(size_t size)
{
	void *rv = malloc(size);
	if (!rv)
		memory_exhausted();
	return rv;
}

void *xrealloc(void *ptr, size_t size)
{
	ptr = realloc(ptr, size);
	if (!ptr)
		memory_exhausted();
	return ptr;
}
