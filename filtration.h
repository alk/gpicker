#ifndef FILTRATION_H
#define FILTRATION_H

#include "scorer.h"

struct filename {
	char *p;
	int dirlength;
	int restlen;
};

char filter_dir_separator;

int dont_sort;
int ignore_positions;

// vector of struct filter_result
struct vector filtered;

typedef void (*filter_destructor)(void *);
typedef int (*filter_func)(struct filename *, const void *, struct filter_result *, unsigned *);

void *prepare_filter(const char *filter, filter_func *func, filter_destructor *destructor);

void filter_files_sync(char *pattern);
void filter_files(char *pattern, void (*callback)(char *));

int obtain_match(const char *pattern, int files_index, unsigned *match);

void uninit_filter(void);

#define FILTER_LIMIT 1000

#endif
