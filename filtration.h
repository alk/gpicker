#ifndef FILTRATION_H
#define FILTRATION_H

#include "scorer.h"

struct filename {
	char *p;
	int dirlength;
};

typedef void (*filter_destructor)(void *);
typedef int (*filter_func)(struct filename *, const void *, struct filter_result *, unsigned *);

void *prepare_filter(const char *filter, filter_func *func, filter_destructor *destructor);

int compare_filter_result(struct filter_result *a, struct filter_result *b);

struct filter_result *filter_files(char *pattern);

#define FILTER_LIMIT 1000

#endif
