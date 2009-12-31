#ifndef FILTRATION_H
#define FILTRATION_H

#include "scorer.h"

struct filename {
	char *p;
	int dirlength;
};

char filter_dir_separator;

// vector of struct filter_result
struct vector filtered;

typedef void (*filter_destructor)(void *);
typedef int (*filter_func)(struct filename *, const void *, struct filter_result *, unsigned *);

void *prepare_filter(const char *filter, filter_func *func, filter_destructor *destructor);

int compare_filter_result(struct filter_result *a, struct filter_result *b);

void filter_files_sync(char *pattern);
void filter_files(char *pattern, void (*callback)(char *));

#define FILTER_LIMIT 1000

#endif
