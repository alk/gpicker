#ifndef XMALLOC_H
#define XMALLOC_H
#include <sys/types.h>

__attribute__((noreturn))
void memory_exhausted(void);
void *xmalloc(size_t size);
void *xrealloc(void *ptr, size_t size);
char *xstrdup(const char *p);

#endif
